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

package uk.gov.hmrc.gform.email

import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import cats.implicits._
import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameter, EmailParametersRecalculated }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class EmailService(emailConnector: EmailConnector) {

  def sendEmail(optemailAddress: Option[String], templateId: String, emailParameters: EmailParametersRecalculated)(
    implicit hc: HeaderCarrier,
    mdc: ExecutionContext): Future[Unit] = {
    Logger.info(s" Sending email, template: $templateId, headers: '${loggingHelpers.cleanHeaderCarrierHeader(hc)}'")
    optemailAddress.fold(().pure[Future])(email => sendEmailTemplate(email, templateId, emailParameters))
  }

  private def sendEmailTemplate(email: String, templateId: String, emailParameters: EmailParametersRecalculated)(
    implicit hc: HeaderCarrier,
    mdc: ExecutionContext) =
    emailConnector.sendEmail(
      new EmailTemplate(
        Seq(email),
        templateId,
        emailParametersRecalculatedToMap(emailParameters)
      ))

  private def emailParametersRecalculatedToMap(emailParameters: EmailParametersRecalculated): Map[String, String] =
    emailParameters.emailParametersMap.map {
      case (emailTemplateVariable, emailParameterValue) =>
        (emailTemplateVariable.emailTemplateVariableId, emailParameterValue.value)
    }

  def getEmailAddress(form: Form): Option[String] =
    form.formData.fields.find(_.id.value == "email").map(_.value).filterNot(_.isEmpty)
}
