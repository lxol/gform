/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.formtemplate

import java.time.LocalDate

import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.core.{ Invalid, Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidatorHelper._
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LangADT }

import scala.Function.const
import scala.collection.immutable.List
import scala.util.{ Failure, Success, Try }

object FormTemplateValidator {

  def validateLanguages(languageList: AvailableLanguages): ValidationResult =
    if (languageList.languages.contains(LangADT.En)) Valid
    else Invalid("languages must contain en")

  private def indexedFields(sections: List[Section]): List[(FormComponentId, Int)] =
    sections.zipWithIndex.flatMap {
      case (section, idx) =>
        val standardFields = section.fields.map(_.id -> idx)
        val subFields = section.fields
          .map(_.`type`)
          .collect {
            case Group(fields, _, _, _, _, _) => fields.map(_.id -> idx)
            case RevealingChoice(options)     => options.toList.flatMap(_.revealingFields.map(_.id -> idx))
          }

        standardFields ::: subFields.flatten
    }

  def someFieldsAreDefinedMoreThanOnce(duplicates: Set[FormComponentId]) =
    s"Some FieldIds are defined more than once: ${duplicates.toList.sortBy(_.value).map(_.value)}"

  def validateUniqueFields(sectionsList: List[Section]): ValidationResult = {
    val fieldIds: List[FormComponentId] = indexedFields(sectionsList).map(_._1)

    val duplicates = fieldIds.groupBy(identity).collect { case (fId, List(_, _, _*)) => fId }.toSet
    duplicates.isEmpty.validationResult(someFieldsAreDefinedMoreThanOnce(duplicates))
  }

  def validateChoiceHelpText(sectionsList: List[Section]): ValidationResult = {
    val choiceFieldIdMap: Map[FormComponentId, Boolean] = sectionsList
      .flatMap(_.fields)
      .map(fv => (fv.id, fv.`type`))
      .collect {
        case (fId, Choice(_, options, _, _, helpTextList @ Some(x :: xs))) =>
          (fId, options.toList.size.equals(helpTextList.getOrElse(List.empty).size))
      }
      .toMap

    val choiceFieldIdResult = choiceFieldIdMap.filter(value => value._2.equals(false))

    choiceFieldIdResult.isEmpty.validationResult(
      s"Choice components doesn't have equal number of choices and help texts ${choiceFieldIdResult.keys.toList}")
  }

  def validateRepeatingSectionFields(sectionList: List[Section]): ValidationResult = {
    val results = sectionList.map { section =>
      (section.repeatsMax, section.repeatsMin) match {
        case (Some(repMax), Some(repMin)) if !repMax.equals(repMin) =>
          Invalid(
            s"The repeatsMax and repeatsMin fields must be the same in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case (Some(_), None) | (None, Some(_)) =>
          val repMax = section.repeatsMax.getOrElse("")
          val repMin = section.repeatsMin.getOrElse("")
          Invalid(
            s"Both repeatsMax and repeatsMin fields must be provided in a repeating section: repeatsMax=[$repMax], repeatsMin=[$repMin]")
        case _ => Valid
      }
    }
    Monoid[ValidationResult].combineAll(results)
  }

  private def getMandatoryAndOptionalFields(section: Section): (Set[FormComponentId], Set[FormComponentId]) =
    SectionHelper.atomicFields(section, Map.empty).foldLeft((Set.empty[FormComponentId], Set.empty[FormComponentId])) {
      case ((mandatoryAcc, optionalAcc), field) =>
        (field.`type`, field.mandatory) match {
          case (Address(_), _) =>
            (mandatoryAcc ++ Address.mandatoryFields(field.id), optionalAcc ++ Address.optionalFields(field.id))
          case (Date(_, _, _), _)        => (mandatoryAcc ++ Date.fields(field.id).toList, optionalAcc)
          case (Text(_, _, _, _), true)  => (mandatoryAcc + field.id, optionalAcc)
          case (Text(_, _, _, _), false) => (mandatoryAcc, optionalAcc + field.id)
          case (_, true)                 => (mandatoryAcc + field.id, optionalAcc)
          case (_, false)                => (mandatoryAcc, optionalAcc + field.id)
        }
    }

  val userContextComponentType: List[FormComponent] => List[FormComponent] =
    enrolledIdentifierComponents =>
      enrolledIdentifierComponents.collect {
        case expr @ HasExpr(SingleExpr(UserCtx(EnrolledIdentifier))) => expr
    }

  def validateEnrolmentIdentifier(formTemplate: FormTemplate): ValidationResult =
    if (userContextComponentType(formTemplate.expandFormTemplate.allFormComponents).nonEmpty) {
      formTemplate.authConfig match {
        case HmrcEnrolmentModule(_) | HmrcAgentWithEnrolmentModule(_, _) => Valid
        case _                                                           => Invalid("You used '${user.enrolledIdentifier}' but you didn't provide 'serviceId'.")
      }
    } else Valid

  def validateEnrolmentSection(formTemplate: FormTemplate): ValidationResult =
    formTemplate.authConfig match {
      case HasEnrolmentSection(_, enrolmentSection, _) =>
        val fcIds = enrolmentSection.fields.map(_.id).map(_.value).toSet
        val ctxs = enrolmentSection.identifiers.map(_.value.value).toList.toSet ++ enrolmentSection.verifiers
          .map(_.value.value)
          .toSet
        ctxs
          .subsetOf(fcIds)
          .validationResult(
            s"Following identifiers and/or verifiers don't have corresponding field entry: " + ctxs
              .diff(fcIds)
              .mkString(", "))
      case _ => Valid
    }

  def validateRegimeId(formTemplate: FormTemplate): ValidationResult = {
    def regimeIdCheck(regimeId: RegimeId): ValidationResult =
      (regimeId.value.length >= 2 && regimeId.value.length <= 8)
        .validationResult("Regime id must be between 2 and 8 characters long")

    formTemplate.authConfig match {
      case HmrcEnrolmentModule(EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) => regimeIdCheck(regimeId)
      case HmrcAgentWithEnrolmentModule(_, EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(regimeId)))) =>
        regimeIdCheck(regimeId)
      case _ => Valid
    }
  }

  def validateDependencyGraph(formTemplate: FormTemplate): ValidationResult = {
    val graph: Graph[FormComponentId, DiEdge] = toGraph(formTemplate)
    constructDepencyGraph(graph).bimap(const(Invalid(s"Graph contains cycle ${graph.findCycle}")), const(Valid)).merge
  }

  def validate(exprs: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = exprs.map(validate(_, formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }

  def validate(componentType: ComponentType, formTemplate: FormTemplate): ValidationResult = componentType match {
    case HasExpr(SingleExpr(expr))     => validate(expr, formTemplate.sections)
    case HasExpr(MultipleExpr(fields)) => Valid
    case Date(_, _, _)                 => Valid
    case Address(_)                    => Valid
    case Choice(_, _, _, _, _)         => Valid
    case RevealingChoice(revealingChoiceElements) =>
      validate(revealingChoiceElements.toList.flatMap(_.revealingFields.map(_.`type`)), formTemplate)
    case HmrcTaxPeriod(_, _, _)    => Valid
    case Group(fvs, _, _, _, _, _) => validate(fvs.map(_.`type`), formTemplate)
    case FileUpload()              => Valid
    case InformationMessage(_, _)  => Valid
  }

  def validateForwardReference(sections: List[Section]): ValidationResult = {
    val fieldNamesIds: Map[FormComponentId, Int] = indexedFields(sections).toMap

    def validateExprs(left: Expr, right: Expr, idx: Int): List[ValidationResult] =
      List(left, right)
        .flatMap(extractFcIds)
        .map(
          id =>
            fieldNamesIds
              .get(id)
              .map(idIdx =>
                (idIdx < idx).validationResult(
                  s"id '${id.value}' named in includeIf is forward reference, which is not permitted"))
              .getOrElse(Invalid(s"id '${id.value}' named in includeIf expression does not exist in a form")))

    def boolean(includeIf: BooleanExpr, idx: Int): List[ValidationResult] = includeIf match {
      case Equals(left, right)              => validateExprs(left, right, idx)
      case NotEquals(left, right)           => validateExprs(left, right, idx)
      case GreaterThan(left, right)         => validateExprs(left, right, idx)
      case GreaterThanOrEquals(left, right) => validateExprs(left, right, idx)
      case LessThan(left, right)            => validateExprs(left, right, idx)
      case LessThanOrEquals(left, right)    => validateExprs(left, right, idx)
      case Not(e)                           => boolean(e, idx)
      case Or(left, right)                  => boolean(left, idx) ::: boolean(right, idx)
      case And(left, right)                 => boolean(left, idx) ::: boolean(right, idx)
      case IsFalse | IsTrue                 => List(Valid)
    }

    Monoid[ValidationResult]
      .combineAll(sections.zipWithIndex.collect {
        case (Section(_, _, _, _, Some(includeIf), _, _, _, _, _, _), idx) => boolean(includeIf.expr, idx)
      }.flatten)
  }

  def validate(expr: Expr, sections: List[Section]): ValidationResult = {
    val fieldNamesIds: List[FormComponentId] = indexedFields(sections).map(_._1)

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = validate(field1, sections)
      val checkField2 = validate(field2, sections)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    expr match {
      case Add(field1, field2)          => checkFields(field1, field2)
      case Subtraction(field1, field2)  => checkFields(field1, field2)
      case Multiply(field1, field2)     => checkFields(field1, field2)
      case Sum(value)                   => validate(value, sections)
      case HmrcRosmRegistrationCheck(_) => Valid
      case FormCtx(value) =>
        fieldNamesIds
          .map(_.value)
          .contains(value)
          .validationResult(s"Form field '$value' is not defined in form template.")
      case AuthCtx(_)          => Valid
      case EeittCtx(_)         => Valid
      case UserCtx(_)          => Valid
      case Constant(_)         => Valid
      case Value               => Valid
      case SubmissionReference => Valid
    }
  }

  def validateEmailParameter(formTemplate: FormTemplate): ValidationResult =
    formTemplate.emailParameters.fold[ValidationResult](Valid) { emailParams =>
      val ids = indexedFields(formTemplate.sections).map(_._1)
      emailParams
        .collect {
          case EmailParameter(_, value: FormCtx) if !ids.contains(value.toFieldId) => value.toFieldId
        } match {
        case Nil => Valid
        case invalidFields =>
          Invalid(
            s"The following email parameters are not fields in the form template's sections: ${invalidFields.mkString(", ")}")
      }
    }

  def validateDates(formTemplate: FormTemplate): ValidationResult =
    getAllDates(formTemplate)
      .map {
        case ConcreteDate(ExactYear(year), ExactMonth(month), ExactDay(day)) =>
          validateYearMonthAndDay(year, month, day)
        case ConcreteDate(AnyYear, ExactMonth(month), ExactDay(day)) =>
          val leapYear = 2020 //makes 29th of feb valid when we dont know the year
          validateYearMonthAndDay(leapYear, month, day)
        case _ => ""
      }
      .filterNot(_ == "") match {
      case messages if messages.isEmpty  => Valid
      case messages if messages.nonEmpty => Invalid(messages.mkString(". "))
    }

  private def validateYearMonthAndDay(year: Int, month: Int, day: Int): String =
    Try(LocalDate.of(year, month, day)) match {
      case Failure(message) => message.toString
      case Success(_)       => ""
    }

  private def extractFcIds(expr: Expr): List[FormComponentId] = expr match {
    case Add(left, right)         => extractFcIds(left) ::: extractFcIds(right)
    case Subtraction(left, right) => extractFcIds(left) ::: extractFcIds(right)
    case Multiply(left, right)    => extractFcIds(left) ::: extractFcIds(right)
    case Sum(field1)              => extractFcIds(field1)
    case id: FormCtx              => List(id.toFieldId)
    case _                        => Nil
  }

  private val isRevealingChoice: FormComponent => Boolean = fc =>
    fc.`type` match {
      case RevealingChoice(_) => true
      case _                  => false
  }

  private val isGroup: FormComponent => Boolean = fc =>
    fc.`type` match {
      case group: Group => true
      case _            => false
  }

  def validateGroup(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Group", formTemplate)(f => {
      case Group(fields, _, _, _, _, _) => fields.forall(f)
    })

  def validateRevealingChoice(formTemplate: FormTemplate): ValidationResult =
    validateComponents("Revealing choice", formTemplate)(f => {
      case RevealingChoice(options) => options.forall(_.revealingFields.forall(f))
    })

  private def validateComponents(str: String, formTemplate: FormTemplate)(
    pf: (FormComponent => Boolean) => PartialFunction[ComponentType, Boolean]): ValidationResult = {

    val formComponents: List[FormComponent] = formTemplate.sections.flatMap(_.fields)

    val rcElements: (FormComponent => Boolean) => Boolean = f =>
      formComponents.map(_.`type`).collect(pf(f)).foldLeft(true)(_ && _)

    val noRevealingChoice = rcElements(fc => !isRevealingChoice(fc))
    val noGroup = rcElements(fc => !isGroup(fc))

    (noRevealingChoice, noGroup) match {
      case (true, true) => Valid
      case (false, _)   => Invalid(str + " cannot contains revealing choice as its element")
      case (_, false)   => Invalid(str + " cannot contains group as its element")
    }
  }
}
