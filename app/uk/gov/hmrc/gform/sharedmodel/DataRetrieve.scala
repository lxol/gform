/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, JsonUtils, OFormatWithTemplateReadFallback }

import scala.util.matching.Regex

case class DataRetrieveId(value: String) extends AnyVal

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
}

sealed trait DataRetrieveAttribute {
  def name: String
}

object DataRetrieveAttribute {

  case object IsValid extends DataRetrieveAttribute {
    override def name: String = "isValid"
  }

  case object Iban extends DataRetrieveAttribute {
    override def name: String = "iban"
  }

  case object AccountNumberIsWellFormatted extends DataRetrieveAttribute {
    override def name: String = "accountNumberIsWellFormatted"
  }

  case object SortCodeIsPresentOnEISCD extends DataRetrieveAttribute {
    override def name: String = "sortCodeIsPresentOnEISCD"
  }

  case object SortCodeBankName extends DataRetrieveAttribute {
    override def name: String = "sortCodeBankName"
  }

  case object NonStandardAccountDetailsRequiredForBacs extends DataRetrieveAttribute {
    override def name: String = "nonStandardAccountDetailsRequiredForBacs"
  }

  case object AccountExists extends DataRetrieveAttribute {
    override def name: String = "accountExists"
  }

  case object NameMatches extends DataRetrieveAttribute {
    override def name: String = "nameMatches"
  }

  case object SortCodeSupportsDirectDebit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectDebit"
  }

  case object SortCodeSupportsDirectCredit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectCredit"
  }

  case object Name extends DataRetrieveAttribute {
    override def name: String = "name"
  }

  case object Status extends DataRetrieveAttribute {
    override def name: String = "status"
  }

  case object RegisteredAddress extends DataRetrieveAttribute {
    override def name: String = "registeredAddress"
  }

  case object RiskScore extends DataRetrieveAttribute {
    override def name: String = "riskScore"
  }

  case object Reason extends DataRetrieveAttribute {
    override def name: String = "reason"
  }

  case object AccountName extends DataRetrieveAttribute {
    override def name: String = "accountName"
  }

  case object EmployerName extends DataRetrieveAttribute {
    override def name: String = "employerName"
  }

  case object SequenceNumber extends DataRetrieveAttribute {
    override def name: String = "sequenceNumber"
  }

  case object WorksNumber extends DataRetrieveAttribute {
    override def name: String = "worksNumber"
  }

  case object TaxDistrictNumber extends DataRetrieveAttribute {
    override def name: String = "taxDistrictNumber"
  }

  case object PayeNumber extends DataRetrieveAttribute {
    override def name: String = "payeNumber"
  }

  case object Director extends DataRetrieveAttribute {
    override def name: String = "director"
  }

  implicit val format: OFormat[DataRetrieveAttribute] = derived.oformat()

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r

  def fromName(name: String): DataRetrieveAttribute = name match {
    case "isValid"                                  => IsValid
    case "accountNumberIsWellFormatted"             => AccountNumberIsWellFormatted
    case "sortCodeIsPresentOnEISCD"                 => SortCodeIsPresentOnEISCD
    case "sortCodeBankName"                         => SortCodeBankName
    case "nonStandardAccountDetailsRequiredForBacs" => NonStandardAccountDetailsRequiredForBacs
    case "accountExists"                            => AccountExists
    case "nameMatches"                              => NameMatches
    case "sortCodeSupportsDirectDebit"              => SortCodeSupportsDirectDebit
    case "sortCodeSupportsDirectCredit"             => SortCodeSupportsDirectCredit
    case "iban"                                     => Iban
    case "name"                                     => Name
    case "status"                                   => Status
    case "registeredAddress"                        => RegisteredAddress
    case "riskScore"                                => RiskScore
    case "reason"                                   => Reason
    case "accountName"                              => AccountName
    case "employerName"                             => EmployerName
    case "sequenceNumber"                           => SequenceNumber
    case "worksNumber"                              => WorksNumber
    case "taxDistrictNumber"                        => TaxDistrictNumber
    case "payeNumber"                               => PayeNumber
    case "director"                                 => Director
    case other                                      => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute name: $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
  def formCtxExprs: List[Expr]
}

object DataRetrieve {

  final case class ValidateBankDetails(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr)
      extends DataRetrieve {
    override def attributes: List[DataRetrieveAttribute] =
      List(
        DataRetrieveAttribute.IsValid,
        DataRetrieveAttribute.SortCodeIsPresentOnEISCD,
        DataRetrieveAttribute.SortCodeBankName,
        DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs,
        DataRetrieveAttribute.SortCodeSupportsDirectCredit,
        DataRetrieveAttribute.SortCodeSupportsDirectDebit,
        DataRetrieveAttribute.Iban
      )
    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber)
  }

  final case class BusinessBankAccountExistence(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    companyName: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      SortCodeIsPresentOnEISCD,
      SortCodeBankName,
      NonStandardAccountDetailsRequiredForBacs,
      AccountExists,
      NameMatches,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit
    )
    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber, accountNumber)
  }

  final case class CompanyRegistrationNumber(
    override val id: DataRetrieveId,
    companyNumber: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      Name,
      Status,
      RegisteredAddress
    )
    override def formCtxExprs: List[Expr] = List(companyNumber)
  }

  final case class NinoInsights(
    override val id: DataRetrieveId,
    nino: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      RiskScore,
      Reason
    )

    override def formCtxExprs: List[Expr] = List(nino)
  }

  final case class BankAccountInsights(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      RiskScore,
      Reason
    )

    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber)
  }

  final case class PersonalBankAccountExistence(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    firstName: Expr,
    lastName: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      AccountExists,
      NameMatches,
      AccountName,
      NonStandardAccountDetailsRequiredForBacs,
      SortCodeIsPresentOnEISCD,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit,
      SortCodeBankName,
      Iban
    )

    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber, firstName, lastName)
  }

  final case class PersonalBankAccountExistenceWithName(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    name: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      AccountExists,
      NameMatches,
      AccountName,
      NonStandardAccountDetailsRequiredForBacs,
      SortCodeIsPresentOnEISCD,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit,
      SortCodeBankName,
      Iban
    )

    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber, name)
  }

  final case class Employments(
    override val id: DataRetrieveId,
    nino: Expr,
    taxYear: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      EmployerName,
      SequenceNumber,
      WorksNumber,
      TaxDistrictNumber,
      PayeNumber,
      Director
    )

    override def formCtxExprs: List[Expr] = List(nino)
  }

  val reads: Reads[DataRetrieve] = new Reads[DataRetrieve] {
    override def reads(json: JsValue): JsResult[DataRetrieve] =
      (for {
        typeValue <- opt[String](json, "type")
        idValue   <- opt[String](json, "id")
        dataRetrieve <- typeValue match {
                          case "validateBankDetails" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                            } yield ValidateBankDetails(DataRetrieveId(idValue), sortCodeExpr, accountNumberExpr)
                          case "businessBankAccountExistence" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              companyNameValue   <- opt[String](parameters, "companyName")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                              companyNameExpr    <- ValueParser.validateWithParser(companyNameValue, ValueParser.expr)
                            } yield BusinessBankAccountExistence(
                              DataRetrieveId(idValue),
                              sortCodeExpr,
                              accountNumberExpr,
                              companyNameExpr
                            )
                          case "personalBankAccountExistence" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                              res <- if ((parameters \ "name").toOption.nonEmpty) {
                                       for {
                                         name <- opt[String](parameters, "name")
                                         name <- ValueParser.validateWithParser(name, ValueParser.expr)
                                       } yield PersonalBankAccountExistenceWithName(
                                         DataRetrieveId(idValue),
                                         sortCodeExpr,
                                         accountNumberExpr,
                                         name
                                       )
                                     } else {
                                       for {
                                         firstName <- opt[String](parameters, "firstName")
                                         lastName  <- opt[String](parameters, "lastName")
                                         firstName <- ValueParser.validateWithParser(firstName, ValueParser.expr)
                                         lastName  <- ValueParser.validateWithParser(lastName, ValueParser.expr)
                                       } yield PersonalBankAccountExistence(
                                         DataRetrieveId(idValue),
                                         sortCodeExpr,
                                         accountNumberExpr,
                                         firstName,
                                         lastName
                                       )
                                     }
                            } yield res
                          case "companyRegistrationNumber" =>
                            for {
                              parameters        <- opt[JsObject](json, "parameters")
                              companyNumber     <- opt[String](parameters, "companyNumber")
                              companyNumberExpr <- ValueParser.validateWithParser(companyNumber, ValueParser.expr)
                            } yield CompanyRegistrationNumber(DataRetrieveId(idValue), companyNumberExpr)
                          case "ninoInsights" =>
                            for {
                              parameters <- opt[JsObject](json, "parameters")
                              nino       <- opt[String](parameters, "nino")
                              ninoExpr   <- ValueParser.validateWithParser(nino, ValueParser.expr)
                            } yield NinoInsights(DataRetrieveId(idValue), ninoExpr)
                          case "employments" =>
                            for {
                              parameters  <- opt[JsObject](json, "parameters")
                              nino        <- opt[String](parameters, "nino")
                              taxYear     <- opt[String](parameters, "taxYear")
                              ninoExpr    <- ValueParser.validateWithParser(nino, ValueParser.expr)
                              taxYearExpr <- ValueParser.validateWithParser(taxYear, ValueParser.expr)
                            } yield Employments(DataRetrieveId(idValue), ninoExpr, taxYearExpr)
                          case "bankAccountInsights" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                            } yield BankAccountInsights(DataRetrieveId(idValue), sortCodeExpr, accountNumberExpr)
                          case other => Left(UnexpectedState(s"'type' value $other not recognized"))
                        }
      } yield dataRetrieve).fold(e => JsError(e.error), r => JsSuccess(r))
  }

  def opt[T](jsValue: JsValue, path: String)(implicit r: Reads[T]): Opt[T] =
    jsValue \ path match {
      case JsDefined(json) =>
        json
          .validate[T]
          .fold(
            invalid => Left(UnexpectedState(s"Type of value is invalid for attribute '$path' [error=$invalid]")),
            valid => Right(valid)
          )
      case _: JsUndefined => Left(UnexpectedState(s"'$path' attribute missing"))
    }

  implicit val format: OFormat[DataRetrieve] = OFormatWithTemplateReadFallback(reads)
}

sealed trait RetrieveDataType extends Product with Serializable

object RetrieveDataType {
  case class ObjectType(data: Map[DataRetrieveAttribute, String]) extends RetrieveDataType
  case class ListType(data: List[Map[DataRetrieveAttribute, String]]) extends RetrieveDataType
}

case class DataRetrieveResult(id: DataRetrieveId, data: RetrieveDataType, requestParams: JsValue)

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieveAttribute, String]] =
    implicitly[Format[Map[String, String]]]
      .bimap[Map[DataRetrieveAttribute, String]](
        _.map { case (key, value) =>
          DataRetrieveAttribute.fromName(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val retrieveDataTypeFormat: Format[RetrieveDataType] = {

    val reads: Reads[RetrieveDataType] = Reads {
      case a: JsArray =>
        implicitly[Reads[List[Map[DataRetrieveAttribute, String]]]].reads(a).map(RetrieveDataType.ListType)
      case other => implicitly[Reads[Map[DataRetrieveAttribute, String]]].reads(other).map(RetrieveDataType.ObjectType)
    }

    val writes: Writes[RetrieveDataType] = Writes[RetrieveDataType] {

      case RetrieveDataType.ObjectType(data) => Json.toJson(data)
      case RetrieveDataType.ListType(data)   => Json.toJson(data)
    }

    Format[RetrieveDataType](reads, writes)
  }
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
