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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import com.fasterxml.jackson.databind.JsonNode
import play.api.libs.json._
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.JsonNodes._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

import scala.collection.JavaConversions._

case class HandlebarsTemplateProcessorModel(model: JsonNode) extends AnyVal {
  def +(that: HandlebarsTemplateProcessorModel): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(fieldMap ++ that.fieldMap)
  private def fieldMap: Map[String, JsonNode] = model.fields.toList.map(e => e.getKey -> e.getValue).toMap
}

object HandlebarsTemplateProcessorModel {
  implicit val format: Format[HandlebarsTemplateProcessorModel] = new Format[HandlebarsTemplateProcessorModel] {
    override def writes(o: HandlebarsTemplateProcessorModel): JsValue = Json.parse(o.model.toString)
    override def reads(json: JsValue): JsResult[HandlebarsTemplateProcessorModel] = JsSuccess(apply(json.toString))
  }

  val empty: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel(objectNode(Map.empty))

  def apply(result: HandlebarsDestinationResponse): HandlebarsTemplateProcessorModel =
    apply(
      Map(
        result.id.id -> objectNode(
          Map("status" -> numberNode(result.status), "json" -> parseJson(result.json.toString)))))

  def apply(jsonDocument: String): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(parseJson(jsonDocument))

  def apply(structuredData: StructuredFormValue.ObjectStructure): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(JsonStructuredFormDataBuilder(structuredData))

  def formId(formId: FormId): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(Map("formId" -> textNode(formId.value)))

  def apply(fields: Map[String, JsonNode]): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(objectNode(fields))

  def apply(fields: (String, JsonNode)*): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(fields.toMap)

  def apply(variables: Variables): HandlebarsTemplateProcessorModel =
    apply(variables.value.toString)

  def apply(status: FormStatus): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(Map("formStatus" -> textNode(status.toString)))

  def hmrcTaxPeriods(form: Form): HandlebarsTemplateProcessorModel = {

    val lookup: Map[FormComponentId, String] = form.formData.fields.map(fd => fd.id -> fd.value).toMap

    def mkMap(od: ObligationDetail): Map[String, String] = Map(
      "periodKey"  -> od.periodKey,
      "periodFrom" -> formatDate(od.inboundCorrespondenceFromDate),
      "periodTo"   -> formatDate(od.inboundCorrespondenceToDate)
    )

    def toJsonNode(taxResponse: TaxResponse) = {
      val fcId = taxResponse.id.recalculatedTaxPeriodKey.fcId
      for {
        periodKey <- lookup.get(fcId)
        obligationDetail <- form.thirdPartyData.obligations
                             .findByPeriodKey(taxResponse.id.recalculatedTaxPeriodKey.hmrcTaxPeriod, periodKey)
      } yield Map(fcId.value -> objectNode(mkMap(obligationDetail).mapValues(textNode)))
    }

    val jsonNodes: Map[String, JsonNode] =
      form.thirdPartyData.obligations match {
        case NotChecked => Map.empty
        case RetrievedObligations(taxResponses) =>
          taxResponses.map(toJsonNode).toList.flatten.foldLeft(Map.empty[String, JsonNode])(_ ++ _)
      }

    HandlebarsTemplateProcessorModel(objectNode(jsonNodes))

  }

  def rosmRegistration(form: Form): HandlebarsTemplateProcessorModel = {
    val f = form.thirdPartyData.desRegistrationResponse.fold("") _

    HandlebarsTemplateProcessorModel(
      "hmrcRosmRegistrationCheck" -> objectNode(Map(
        "safeId"           -> f(_.safeId),
        "organisationName" -> f(_.orgOrInd.getOrganisationName),
        "organisationType" -> f(_.orgOrInd.getOrganisationType),
        "isAGroup"         -> f(_.orgOrInd.getIsAGroup)
      ).mapValues(textNode)))
  }
}
