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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DisplayWidth, Expr, FormatExpr, ShortText, Text, TextExpression, TextFormat, UkSortCode, UkSortCodeFormat, Value, ValueExpr }
import cats.syntax.either._

object FormComponentMakerService {

  def createObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    displayWidth: Option[String],
    toUpperCase: Option[String]) =
    (maybeFormatExpr, maybeValueExpr, multiLine, displayWidth) match {
      case (Some(TextFormat(UkSortCodeFormat)), HasTextExpression(expr), IsNotMultiline(), _) =>
        UkSortCode(expr).asRight
        textOrTextAreaMatch(maybeFormatExpr, maybeValueExpr, multiLine, displayWidth, toUpperCase)
    }

  private def textOrTextAreaMatch(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    displayWidth: Option[String],
    toUpperCase: Option[String]) = multiLine match {
    case IsNotMultiline() => createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
  }

  def createTextObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String],
    toUpperCase: Option[String]) = (maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase) match {
    case (Some(TextFormat(f)), HasTextExpression(expr), None, _)                => Text(f, expr).asRight
    case (None, HasTextExpression(expr), None, _)                               => Text(ShortText, expr).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw), _) => Text(f, expr, dw).asRight
    case (None, HasTextExpression(expr), HasDisplayWidth(dw), _)                => Text(ShortText, expr, dw).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw), _) => Text(f, expr, dw).asRight
    case (None, HasTextExpression(expr), HasDisplayWidth(dw), _)                => Text(ShortText, expr, dw).asRight
  }

  final object HasTextExpression {
    def unapply(valueExp: Option[ValueExpr]): Option[Expr] =
      valueExp match {
        case Some(TextExpression(expr)) => Some(expr)
        case None                       => Some(Value)
        case _                          => None
      }
  }

  final object IsMultiline {
    def unapply(multiline: Option[String]): Boolean =
      multiline match {
        case Some(IsTrueish()) => true
        case _                 => false
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

  object IsTrueish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "true" | "yes" => true
        case _              => false
      }
  }
  final object ToUpperCase {
    def unapply(isUpperCase: Option[String]): Option[Boolean] =
      isUpperCase match {
        case Some("true")  => Some(true)
        case Some("false") => Some(false)
        case _             => Some(false)
      }
  }
}
