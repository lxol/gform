/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.core.parsers

import java.time.LocalDate

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveAttribute, DataRetrieveId }

object ValueParser {

  implicit val W = Whitespace(() | """\s+""".r)

  def validate(expression: String): Opt[ValueExpr] =
    validateWithParser(expression, exprDeterminer).right.map(_.rewrite)

  lazy val exprDeterminer: Parser[ValueExpr] = (dateExpression ^^ ((loc, expr) => DateExpression(expr))
    | positiveIntegers ^^ ((loc, selections) => ChoiceExpression(selections))
    | expr ^^ ((loc, expr) => TextExpression(expr)))

  lazy val dateExpression: Parser[DateValue] = (nextDate | lastDate | exactDate | today) ^^ { (loc, dateExpr) =>
    dateExpr
  }

  lazy val today: Parser[TodayDateValue.type] = "today" ^^ const(TodayDateValue)

  lazy val exactDate: Parser[ExactDateValue] = exactYearParser ~ exactMonthDay ^^ { (loc, year, month, day) =>
    ExactDateValue(year, month, day)
  } | exactYearMonth ~ "firstDay" ^^ { (loc, year, month, _) =>
    ExactDateValue(year, month, 1)
  } | exactYearMonth ~ "lastDay" ^^ { (loc, year, month, _) =>
    ExactDateValue(year, month, LocalDate.of(year, month, 1).lengthOfMonth)
  }

  lazy val nextDate: Parser[NextDateValue] =
    nextOrPreviousValue("next", NextDateValue.apply)

  lazy val lastDate: Parser[PreviousDateValue] =
    nextOrPreviousValue("last", PreviousDateValue.apply)

  lazy val exprFormCtx: Parser[Expr] = (quotedLocalisedConstant
    | parserExpression)

  lazy val dateExprExactParser: Parser[DateExpr] = exactDayParser ~ exactMonthParser ~ exactYearParser ^^ {
    (_, day, month, year) =>
      DateValueExpr(ExactDateExprValue(year, month, day))
  }

  lazy val dateExprExactQuoted: Parser[DateExpr] = "'" ~ dateExprExactParser ~ "'" ^^ { (_, _, dateExpr, _) =>
    dateExpr
  } | dateExprExactParser

  lazy val signedInt: Parser[Int] = plusOrMinus ~ positiveInteger ^^ { (_, plusOrMinus, i) =>
    i * (plusOrMinus match {
      case "+" => 1
      case "-" => -1
    })
  }

  lazy val signedYear: Parser[OffsetUnit] = signedInt ~ "y" ^^ { (_, year, _) =>
    OffsetUnit.Year(year)
  }

  lazy val signedMonth: Parser[OffsetUnit] = signedInt ~ "m" ^^ { (_, month, _) =>
    OffsetUnit.Month(month)
  }

  lazy val signedDay: Parser[OffsetUnit] = signedInt ~ "d" ^^ { (_, day, _) =>
    OffsetUnit.Day(day)
  }

  private val offsets = List(signedYear, signedMonth, signedDay)

  private val perms1: List[Parser[OffsetYMD]] = offsets.map { ap =>
    ap ^^ { (_, a) => OffsetYMD(a) }
  }

  private val perms2: Iterator[Parser[OffsetYMD]] =
    offsets.combinations(2).flatMap(_.permutations).map { case List(ap, bp) =>
      ap ~ bp ^^ { (_, a, b) =>
        OffsetYMD(a, b)
      }
    }

  private val perms3: Iterator[Parser[OffsetYMD]] = offsets.permutations.map { case List(ap, bp, cp) =>
    ap ~ bp ~ cp ^^ { (_, a, b, c) =>
      OffsetYMD(a, b, c)
    }
  }

  private val allYMDVariations = perms1 ++ perms2 ++ perms3

  lazy val offsetYMD: Parser[OffsetYMD] = allYMDVariations.reduce(_ | _)

  lazy val dateExprTODAY: Parser[DateExpr] = "TODAY" ^^^ DateValueExpr(TodayDateExprValue)

  lazy val dateExprTODAYOffset: Parser[DateExpr] = dateExprTODAY ~ offsetYMD ^^ { (_, dateExprToday, offsetYMD) =>
    DateExprWithOffset(dateExprToday, offsetYMD)
  } | dateExprTODAY

  lazy val formCtxFieldDateWithOffset: Parser[DateExprWithOffset] = formCtxFieldDate ~ offsetYMD ^^ {
    (_, dateExprCtx, offsetYMD) =>
      DateExprWithOffset(dateExprCtx, offsetYMD)
  }

  lazy val dateExpr: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDate | formCtxFieldDateWithOffset

  lazy val dateExprWithoutFormCtxFieldDate: Parser[DateExpr] =
    dateExprExactQuoted | dateExprTODAYOffset | formCtxFieldDateWithOffset

  lazy val dataSourceParse: Parser[DataSource] = (
    "service" ~ "." ~ "seiss" ^^ { (_, _, _, _) =>
      DataSource.SeissEligible
    }
      | "mongo" ~ "." ~ alphabeticOnly ^^ { (_, _, _, name) =>
        DataSource.Mongo(CollectionName(name))
      }
      | "user" ~ "." ~ "enrolments" ~ "." ~ enrolment ^^ { (_, _, _, _, _, enrolment) =>
        DataSource.Enrolment(enrolment.serviceName, enrolment.identifierName)
      }
      | "delegated.classic.enrolments." ~ enrolment ^^ { (_, _, enrolment) =>
        DataSource.DelegatedEnrolment(enrolment.serviceName, enrolment.identifierName)
      }
  )

  lazy val expr: Parser[Expr] = (quotedConstant
    | "${" ~> parserExpression <~ "}")

  lazy val internalLinkParser: Parser[InternalLink] = "printAcknowledgementPdf" ^^ { (loc, _) =>
    InternalLink.printAcknowledgementPdf
  } | "printSummaryPdf" ^^ { (loc, _) =>
    InternalLink.printSummaryPdf
  } | "newForm" ~ "." ~ FormTemplateId.unanchoredIdValidation ^^ { (loc, _, _, id) =>
    InternalLink.NewFormForTemplate(FormTemplateId(id))
  } | "newForm" ^^ { (loc, _) =>
    InternalLink.newForm
  } | "newSession" ^^ { (loc, _) =>
    InternalLink.newSession
  } | PageId.unanchoredIdValidation ^^ { (loc, id) =>
    PageLink(PageId(id))
  }

  private lazy val periodFun = "period(" ~ dateExpr ~ "," ~ dateExpr ~ ")"

  private lazy val userFieldFunc: Parser[UserFieldFunc] = "count" ^^ { (_, _) =>
    UserFieldFunc.Count
  } | nonZeroPositiveInteger ^^ { (_, i) =>
    UserFieldFunc.Index(i)
  }

  private lazy val userEnrolmentFunc: Parser[UserCtx] =
    "user" ~ "." ~ userFieldEnrolments ~ "." ~ userFieldFunc ^^ { (_, _, _, userField, _, func) =>
      UserCtx(Enrolment(userField.serviceName, userField.identifierName, Some(func)))
    }

  lazy val contextField: Parser[Expr] = userEnrolmentFunc | ("user" ~ "." ~ userField ^^ { (loc, _, _, userField) =>
    UserCtx(userField)
  }
    | "form" ~ "." ~ "submissionReference" ^^ { (loc, _, _, fieldName) =>
      FormTemplateCtx(FormTemplateProp.SubmissionReference)
    }
    | "form" ~ "." ~ "id" ^^ { (loc, _, _, fieldName) =>
      FormTemplateCtx(FormTemplateProp.Id)
    }
    | "form" ~ "." ~ "lang" ^^ { (loc, _, _, fieldName) =>
      LangCtx
    }
    | "form" ~ "." ~ FormComponentId.unanchoredIdValidation ^^ { (loc, _, _, fieldName) =>
      FormCtx(FormComponentId(fieldName))
    }
    | "param" ~ "." ~ alphabeticOnly ^^ { (loc, _, _, param) =>
      ParamCtx(QueryParam(param))
    }
    | "auth" ~ "." ~ authInfo ^^ { (loc, _, _, authInfo) =>
      AuthCtx(authInfo)
    }
    | "hmrcRosmRegistrationCheck" ~ "." ~ rosmProp ^^ { (loc, _, _, rosmProp) =>
      HmrcRosmRegistrationCheck(rosmProp)
    }
    | "link" ~ "." ~ internalLinkParser ^^ { (loc, _, _, internalLink) =>
      LinkCtx(internalLink)
    }
    | "dataRetrieve" ~ "." ~ DataRetrieveId.unanchoredIdValidation ~ "." ~ DataRetrieveAttribute.unanchoredIdValidation ^^ {
      (loc, _, _, dataRetrieveId, _, dataRetrieveAttribute) =>
        DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieveAttribute.fromName(dataRetrieveAttribute))
    }
    | dateExprWithoutFormCtxFieldDate.map(
      DateCtx.apply
    ) // to parse date form fields with offset or date constants i.e TODAY, 01012020 etc (with or without offset)
    | periodValueParser ^^ { (loc, period) =>
      PeriodValue(period)
    }
    | (periodFun ~ "." ~ "sum|totalMonths|years|months|days".r mapWithLines {
      case (loc, _ ~ (dateExpr1: DateExpr) ~ _ ~ (dateExpr2: DateExpr) ~ _ ~ _ ~ prop) =>
        PeriodExt(
          Period(DateCtx(dateExpr1), DateCtx(dateExpr2)),
          prop match {
            case "sum"         => PeriodFn.Sum
            case "totalMonths" => PeriodFn.TotalMonths
            case "years"       => PeriodFn.Years
            case "months"      => PeriodFn.Months
            case "days"        => PeriodFn.Days
            case _ =>
              throw new IllegalArgumentException(
                "period(*,*).prop value is invalid. Allowed prop values are sum,totalMonths,years,months,days"
              )
          }
        )
    })
    | periodFun ^^ { (loc, _, dateExpr1, _, dateExpr2, _) =>
      Period(DateCtx(dateExpr1), DateCtx(dateExpr2))
    }
    | quotedLocalisedConstant
    | FormComponentId.unanchoredIdValidation ~ ".sum" ^^ { (loc, value, _) =>
      Sum(FormCtx(FormComponentId(value)))
    }
    | FormComponentId.unanchoredIdValidation ~ ".count" ^^ { (loc, value, _) =>
      Count(FormComponentId(value))
    }
    | FormComponentId.unanchoredIdValidation ~ "." ~ addressDetail ^^ { (loc, value, _, addressDetail) =>
      AddressLens(FormComponentId(value), addressDetail)
    }
    | anyDigitConst ^^ { (loc, str) =>
      str
    }
    | FormComponentId.unanchoredIdValidation ^^ { (loc, fn) =>
      FormCtx(FormComponentId(fn))
    })

  lazy val addressDetail: Parser[AddressDetail] =
    "line1" ^^^ AddressDetail.Line1 |
      "line2" ^^^ AddressDetail.Line2 |
      "line3" ^^^ AddressDetail.Line3 |
      "line4" ^^^ AddressDetail.Line4 |
      "postcode" ^^^ AddressDetail.Postcode |
      "country" ^^^ AddressDetail.Country

  lazy val formCtxFieldDate: Parser[DateExpr] = "form" ~ "." ~ FormComponentId.unanchoredIdValidation ^^ {
    (_, _, _, fieldName) =>
      DateFormCtxVar(FormCtx(FormComponentId(fieldName)))
  } | FormComponentId.unanchoredIdValidation ^^ { (_, fn) =>
    DateFormCtxVar(FormCtx(FormComponentId(fn)))
  }

  lazy val parserExpression: Parser[Expr] = "(" ~ addExpression ~ ")" ^^ { (loc, _, expr, _) =>
    expr
  } | addExpression

  lazy val addExpression: Parser[Expr] = (parserExpression ~ "+" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Add(expr1, expr2)
  }
    | subtractionExpression)

  lazy val subtractionExpression: Parser[Expr] =
    (parserExpression ~ "-" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
      Subtraction(expr1, expr2)
    }
      | product)

  lazy val product: Parser[Expr] = (parserExpression ~ "*" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Multiply(expr1, expr2)
  }
    | ifElseParser)

  lazy val ifElseParser: Parser[Expr] = {
    ("if" ~> BooleanExprParser.p4 ~ "then" ~ parserExpression ~ "else" ~ parserExpression ^^ {
      (_, cond, _, expr1, _, expr2) =>
        IfElse(cond, expr1, expr2)
    } | orElseParser)
  }

  lazy val orElseParser: Parser[Expr] = parserExpression ~ "else" ~ parserExpression ^^ { (loc, expr1, _, expr2) =>
    Else(expr1, expr2)
  } | contextField ^^ { (loc, value) =>
    value
  }

  lazy val alphabeticOnly: Parser[String] = """[a-zA-Z]\w*""".r ^^ { (loc, str) =>
    str
  }

  lazy val quotedLocalisedConstant: Parser[Expr] = (quotedConstant ~ "," ~ quotedConstant ^^ { (_, en, _, cy) =>
    IfElse(Equals(LangCtx, Constant("en")), en, cy)
  }
    |
    quotedConstant)

  lazy val quotedConstant: Parser[Expr] = ("'" ~ anyConstant ~ "'" ^^ { (loc, _, str, _) =>
    str
  }
    |
    "''".r ^^ { (loc, value) =>
      Constant("")
    })

  lazy val anyConstant: Parser[Constant] = """[^']+""".r ^^ { (loc, str) =>
    Constant(str)
  }

  lazy val anyDigitConst: Parser[Expr] = (
    // parse single digit, e.g. "9", "+9"
    """[+-]?\d""".r ^^ { (loc, str) =>
      Constant(str)
    }
    // parse two or more digits with commas as thousands separators digit, e.g. "9,876"
      | """[+-]?\d[\d,]*[\d]""".r ^^ { (loc, str) =>
        Constant(str)
      }
      // parse decimal fraction, e.g. ".56"
      | """[+-]?\.[\d]*\d""".r ^^ { (loc, str) =>
        Constant(str)
      }
      // parse number plus decimal fraction, e.g. "1,234.56"
      | """[+-]?\d[\d,]*\.([\d]*\d)?""".r ^^ { (loc, str) =>
        Constant(str)
      }
  )

  lazy val userFieldEnrolments: Parser[UserField.Enrolment] = "enrolments" ~ "." ~ enrolment ^^ ((_, _, _, en) => en)

  lazy val userField: Parser[UserField] = (
    "affinityGroup" ^^ const(UserField.AffinityGroup)
      | userFieldEnrolments
      | "enrolledIdentifier" ^^ const(UserField.EnrolledIdentifier)
  )

  lazy val enrolment: Parser[UserField.Enrolment] = serviceName ~ "." ~ identifierName ^^ { (_, sn, _, in) =>
    UserField.Enrolment(sn, in, None)
  }

  lazy val serviceName: Parser[ServiceName] = """[^.!= }]+""".r ^^ { (loc, str) =>
    ServiceName(str)
  }

  lazy val identifierName: Parser[IdentifierName] = """[^.!= }]+""".r ^^ { (loc, str) =>
    IdentifierName(str)
  }

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ const(GG)
      | "payenino" ^^ const(PayeNino)
      | "sautr" ^^ const(SaUtr)
      | "ctutr" ^^ const(CtUtr)
      | "email" ^^ const(EmailId)
  )
  lazy val rosmProp: Parser[RosmProp] = (
    "safeId" ^^ const(RosmSafeId)
      | "organisationName" ^^ const(RosmOrganisationName)
      | "organisationType" ^^ const(RosmOrganisationType)
      | "isAGroup" ^^ const(RosmIsAGroup)
  )

  private def const[A](a: A)(loc: List[Line], matched: String): A = a
}
