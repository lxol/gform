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

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.time.TimeProvider
import java.time.format.DateTimeFormatter

import com.github.jknack.handlebars.{ Handlebars, Options }

class HandlebarsTemplateProcessorHelpers(timeProvider: TimeProvider = new TimeProvider) {
  def yesNoToEtmpChoice(yesNoChoice: String): CharSequence = ifNotNull(yesNoChoice) {
    case "1" => condition("0")
    case "0" => condition("1")
  }

  def dateToEtmpDate(date: String): CharSequence =
    ifNotNull(date) { d =>
      condition(d.substring(0, 4) + d.substring(5, 7) + d.substring(8, 10))
    }

  // Due to the way Handlebars works, when this helper is used, an
  // initial dummy parameter must be passed before the list of values
  // to select from. Anything will do. I suggest 'null' or 'this'.
  def either(o: Options): CharSequence = eitherN(o.params.toSeq: _*)

  // Handlebars.java can't deal with a varargs argument in a helper.
  // Neither can it deal with overloading, so we'd need to do something like
  // either2, either3, either4, etc.
  def either2(arg1: Any, arg2: Any): CharSequence = eitherN(arg1, arg2)
  def either3(arg1: Any, arg2: Any, arg3: Any): CharSequence = eitherN(arg1, arg2, arg3)
  def either4(arg1: Any, arg2: Any, arg3: Any, arg4: Any): CharSequence = eitherN(arg1, arg2, arg3, arg4)
  def either5(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5)
  def either6(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6)
  def either7(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  def either8(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  def either9(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any, arg9: Any)
    : CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  def either10(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  def either11(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  def either12(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  def either13(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  def either14(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  def either15(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  def either16(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  def either17(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any): CharSequence =
    eitherN(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17)
  def either18(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any): CharSequence =
    eitherN(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18)
  def either19(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any,
    arg19: Any): CharSequence =
    eitherN(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19)
  def either20(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any,
    arg19: Any,
    arg20: Any): CharSequence =
    eitherN(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19,
      arg20)

  private[handlebars] def eitherN(args: Any*): CharSequence = condition(args.find(_ != null).orNull)

  def isSuccessCode(code: Integer): CharSequence = isSuccCode(code, "true", "false")
  def isNotSuccessCode(code: Integer): CharSequence = isSuccCode(code, "false", "true")
  private def isSuccCode(code: Integer, t: String, f: String): CharSequence =
    if (code == null) "false"
    else if (code >= 200 && code <= 299) t
    else f

  def getCurrentDate: CharSequence = condition(DateTimeFormatter.BASIC_ISO_DATE.format(timeProvider.localDateTime))
  def getCurrentTimestamp: CharSequence = condition(DateTimeFormatter.ISO_INSTANT.format(timeProvider.instant))

  def getHmrcTaxPeriodKey(period: String): CharSequence = getHmrcTaxPeriodValue(period, 0)
  def getHmrcTaxPeriodFrom(period: String): CharSequence = getHmrcTaxPeriodValue(period, 1)
  def getHmrcTaxPeriodTo(period: String): CharSequence = getHmrcTaxPeriodValue(period, 2)
  private def getHmrcTaxPeriodValue(period: String, index: Int): CharSequence =
    ifNotNull(period) { s =>
      condition(s.split('|')(index))
    }

  def toEtmpLegalStatus(frontEndValue: String): CharSequence = ifNotNull(frontEndValue) { s =>
    condition(s.toLowerCase match {
      case "sole trader" | "sole proprietor"         => "1"
      case "limited liability partnership"           => "2"
      case "partnership"                             => "3"
      case "unincorporated body" | "local authority" => "5"
      case "trust"                                   => "6"
      case "public corporation" | "limited company"  => "7"
    })
  }

  def toEtmpDeclarationStatus(frontEndValue: String): CharSequence = ifNotNull(frontEndValue) { s =>
    condition(s.toLowerCase match {
      case "authorised official"             => "1"
      case "company secretary"               => "2"
      case "director"                        => "3"
      case "partner"                         => "4"
      case "sole proprietor" | "sole trader" => "5"
      case "trustee"                         => "6"
      case "others"                          => "7"
    })
  }

  private def ifNotNull[T](t: T)(f: T => CharSequence): CharSequence =
    Option(t).map(f).getOrElse("null")

  private def condition(v: Any): CharSequence =
    new Handlebars.SafeString(Option(v) match {
      case None            => "null"
      case Some(s: String) => s.replaceAll("""\\""", """\\\\""").replaceAll("""'""", """\\'""")
      case Some(u)         => u.toString
    })
}
