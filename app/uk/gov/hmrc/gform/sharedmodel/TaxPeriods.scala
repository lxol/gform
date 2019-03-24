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

package uk.gov.hmrc.gform.sharedmodel

import cats.data.NonEmptyList
import java.util.Date

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class ObligationDetail(
  status: String,
  inboundCorrespondenceFromDate: Date,
  inboundCorrespondenceToDate: Date,
  inboundCorrespondenceDueDate: Date,
  periodKey: String)

object ObligationDetail {
  implicit val format: OFormat[ObligationDetail] = Json.format[ObligationDetail]
}

case class ObligationDetails(obligationDetails: List[ObligationDetail])

object ObligationDetails {
  implicit val format: OFormat[ObligationDetails] = Json.format[ObligationDetails]
}

case class Obligation(obligations: List[ObligationDetails])

object Obligation {
  implicit val format: OFormat[Obligation] = Json.format[Obligation]
}

case class TaxResponse(id: HmrcTaxPeriodWithEvaluatedId, obligation: Obligation)

object TaxResponse {
  implicit val format: OFormat[TaxResponse] = Json.format[TaxResponse]
}

case class ObligationsResponse(obligationsResponse: Option[NonEmptyList[TaxResponse]])

object ObligationsResponse {
  import JsonUtils._
  implicit val format: OFormat[ObligationsResponse] = Json.format[ObligationsResponse]
}

case class IdNumberValue(value: String) extends AnyVal

object IdNumberValue {
  implicit val format: OFormat[IdNumberValue] = derived.oformat
}

case class HmrcTaxPeriodWithEvaluatedId(
  fcId: FormComponentId,
  hmrcTaxPeriod: HmrcTaxPeriod,
  idNumberValue: IdNumberValue)

object HmrcTaxPeriodWithEvaluatedId {
  implicit val format: OFormat[HmrcTaxPeriodWithEvaluatedId] = derived.oformat
}
