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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BasicText, ComponentType, DisplayWidth, Expr, FormatExpr, ShortText, Text, TextArea, TextExpression, TextFormat, UkSortCode, UkSortCodeFormat, Value, ValueExpr }
import cats.syntax.either._
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.exceptions.UnexpectedState

object FormComponentMakerService {

  def createObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    displayWidth: Option[String],
    toUpperCase: Option[String],
    json: Option[JsValue]): Either[UnexpectedState, ComponentType] =
    (maybeFormatExpr, maybeValueExpr, multiLine, displayWidth, json) match {
      case (Some(TextFormat(UkSortCodeFormat)), HasTextExpression(expr), IsNotMultiline(), _, None) =>
        UkSortCode(expr).asRight
      case (_, _, IsNotMultiline(), _, None) =>
        createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
      case (_, _, IsMultiline(), _, None) => createTextAreaObject(maybeFormatExpr, maybeValueExpr, displayWidth)
      case (_, _, _, _, Some(json))       => Left(createError(maybeFormatExpr, maybeValueExpr, multiLine, json))
      case (_, _, _, _, _) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field bing
                            |""".stripMargin).asLeft
    }

//  type Parameter = (Option[FormatExpr], Option[ValueExpr], Option[String], Option[String], Option[String])
//  type Result = Either[UnexpectedState, ComponentType]
//  type PF =  PartialFunction[Parameter, Result]
//
//  private val foo: PF = {
//    case (maybeFormatExpr, maybeValueExpr, IsNotMultiline(), displayWidth, toUpperCase) => createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
//    case (maybeFormatExpr, maybeValueExpr, IsMultiline(), displayWidth, _)              => createTextAreaObject(maybeFormatExpr, maybeValueExpr, displayWidth)
//  }
//
//  private val createTexObj: PF = {
//    case (maybeFormatExpr,maybeValueExpr,IsNotMultiline(), HasDisplayWidth(dw),ToUpperCase(a)) =>
//  }

//  private def textOrTextAreaMatch(
//    maybeFormatExpr: Option[FormatExpr],
//    maybeValueExpr: Option[ValueExpr],
//    multiLine: Option[String],
//    displayWidth: Option[String],
//    toUpperCase: Option[String]) = multiLine match {
//    case IsNotMultiline() => createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
//    case IsMultiline()    => createTextAreaObject(maybeFormatExpr, maybeValueExpr, displayWidth)
//  }

//  private def createErrorOrTextAreaMatch(
//    maybeFormatExpr: Option[FormatExpr],
//    maybeValueExpr: Option[ValueExpr],
//    multiLine: Option[String],
//    displayWidth: Option[String],
//    toUpperCase: Option[String]) = multiLine match {
//    case IsNotMultiline() => createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
//    case IsMultiline()    => createTextAreaObject(maybeFormatExpr, maybeValueExpr, displayWidth)
//  }

  def createTextObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String],
    toUpperCase: Option[String]) = (maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase) match {

    case (Some(TextFormat(f)), HasTextExpression(expr), None, ToUpperCase(a)) =>
      Text(f, expr, DisplayWidth.DEFAULT, a).asRight
    case (None, HasTextExpression(expr), None, ToUpperCase(a)) =>
      Text(ShortText, expr, DisplayWidth.DEFAULT, a).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw), ToUpperCase(a)) =>
      Text(f, expr, dw, a).asRight
    case (None, HasTextExpression(expr), HasDisplayWidth(dw), ToUpperCase(a)) =>
      Text(ShortText, expr, dw, a).asRight
  }

  def createTextAreaObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String]) = (maybeFormatExpr, maybeValueExpr, displayWidth) match {

    case (Some(TextFormat(f)), HasTextExpression(expr), None) => TextArea(f, expr).asRight
    case (None, HasTextExpression(expr), None)                => TextArea(BasicText, expr).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw)) =>
      TextArea(f, expr, dw).asRight
    case (None, HasTextExpression(expr), HasDisplayWidth(dw)) => TextArea(BasicText, expr, dw).asRight
  }

  def createError(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    json: JsValue): UnexpectedState = {
    val formComponentMaker = new FormComponentMaker(json)
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      case (maybeInvalidFormat, maybeInvalidValue, IsMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format or value for multiline text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeInvalidValue
                            |""".stripMargin)
      case (Some(invalidFormat), None, IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $invalidFormat
                            |Value: must supply a value
                            |""".stripMargin)
      case (None, Some(invalidValue), IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: "must supply a value for format"
                            |Value: $invalidValue
                            |""".stripMargin)
      case (Some(invalidFormat), Some(invalidValue), IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $invalidFormat
                            |Value: $invalidValue
                            |""".stripMargin)
    }
  }

  final object HasTextExpression {
    def unapply(valueExp: Option[ValueExpr]): Option[Expr] =
      valueExp match {
        case Some(TextExpression(expr)) => Some(expr)
        case None                       => Some(Value)
        case _                          => None
      }
  }

  final object IsNotMultiline {
    def unapply(multiline: Option[String]): Boolean = !IsMultiline.unapply(multiline)
  }

  final object HasDisplayWidth {
    def unapply(displayWidth: Option[String]): Option[DisplayWidth] =
      displayWidth match {
        case Some("xs")  => Some(DisplayWidth.XS)
        case Some("s")   => Some(DisplayWidth.S)
        case Some("m")   => Some(DisplayWidth.M)
        case Some("l")   => Some(DisplayWidth.L)
        case Some("xl")  => Some(DisplayWidth.XL)
        case Some("xxl") => Some(DisplayWidth.XXL)
        case _           => Some(DisplayWidth.DEFAULT)
      }
  }

  final object ToUpperCase {
    def unapply(isUpperCase: Option[String]): Option[Boolean] =
      isUpperCase match {
        case Some(IsTrueish()) => Some(true)
        case _                 => Some(false)
      }
  }

  final object IsMultiline {
    def unapply(multiline: Option[String]): Boolean =
      multiline match {
        case Some(IsTrueish()) => true
        case _                 => false
      }
  }

  object IsTrueish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "true" | "yes" => true
        case _              => false
      }
  }
}

object a extends App {
  val sample = 1 to 5

  val isEven: PartialFunction[Int, String] = {
    case x if x % 2 == 0 => x + " is even"
  }

  val evenNumbers = sample collect isEven

  val isOdd: PartialFunction[Int, String] = {
    case x if x % 2 == 1 => x + " is odd"
  }
  val numbers = sample map (isEven orElse isOdd)
}
