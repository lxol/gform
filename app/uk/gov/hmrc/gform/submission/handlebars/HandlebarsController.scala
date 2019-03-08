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

import play.api.libs.json._
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, Variables }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsController(formService: FormService, templateService: FormTemplateService) extends BaseController {

  def handlebarsModelWithFrontEndVariables(formId: FormId): Action[Variables] = Action.async(parse.json[Variables]) {
    implicit request =>
      toJson {
        modelFromFormData(formId).map(_ + HandlebarsTemplateProcessorModel(request.body))
      }
  }

  def handlebarsModelWithoutFrontEndVariables(formId: FormId): Action[AnyContent] = Action.async { implicit request =>
    toJson(modelFromFormData(formId))
  }

  private def toJson(model: Future[HandlebarsTemplateProcessorModel])(implicit ec: ExecutionContext) =
    model.map(_.model.toString).map(Json.parse).asOkJson

  private def modelFromFormData(formId: FormId)(implicit hc: HeaderCarrier) =
    for {
      form     <- formService.get(formId)
      template <- templateService.get(form.formTemplateId)
    } yield HandlebarsTemplateProcessorModel(form, template)
}
