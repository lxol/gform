/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileupload

import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future
import uk.gov.hmrc.http.{ BadRequestException, HeaderCarrier, HttpResponse, RequestEntityTooLargeException }

class RouteException(val message: String) extends Exception(message)

class FileUploadConnector(config: FUConfig, wSHttp: WSHttp, timeProvider: TimeProvider) {
  val helper = new Helper(config, timeProvider)

  def createEnvelope(formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): Future[EnvelopeId] = {
    Logger.info(
      s"creating envelope, formTemplateId: '${formTemplateId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    val requestBody = helper.createEnvelopeRequestBody(formTemplateId)
    wSHttp
      .POST(s"$baseUrl/file-upload/envelopes", requestBody, headers)
      .map(helper.extractEnvelopId)
  }

  def routeEnvelope(input: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    Logger.info(s"route envelope, input: '${input.envelopeId.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)} ")
    wSHttp
      .POST[RouteEnvelopeRequest, HttpResponse](s"$baseUrl/file-routing/requests", input, headers)
      // Debug code
      // Simulate routing failure
//      .flatMap(_ => {
//        val x = 1
//        val xx = x
//        Future.failed(new BadRequestException("""{"error":{"msg":"Envelope size exceeds maximum of 26.00 MB"}}"""))
//      })
      // End of debug code
      .recover {
        case e: BadRequestException if e.message.contains("Envelope size exceeds maximum") => {
          throw new RouteException("Envelope size exceeds maximum")
        }
      }
      .map(_ => ())
  }

  def unsealEnvelope(input: UnsealEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    Logger.info(s"unseal envelope, input: '${input.id.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)} ")
    wSHttp
      .POST[UnsealEnvelopeRequest, HttpResponse](s"$baseUrl/file-upload/commands/unsealenvelope", input, headers)
      .map(_ => ())
  }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] = {
    Logger.info(s"get envelope, envelopeId: '${envelopeId.value}', ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    wSHttp.GET[Envelope](s"$baseUrl/file-upload/envelopes/${envelopeId.value}")
  }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] = {
    Logger.info(s"delete file, envelopeId: ' ${envelopeId.value}', fileId: '${fileId.value}', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")
    wSHttp
      .DELETE[HttpResponse](s"$baseUrl/file-upload/envelopes/${envelopeId.value}/files/${fileId.value}")
      .map(_ => ())
  }
  private lazy val baseUrl = config.fileUploadBaseUrl
  private lazy val `Csrf-Token: nocheck` = "Csrf-Token" -> "nocheck"

  /**
    * TIP. The Crsf-Token is not needed on production. It's as well not intrusive.
    * We're adding it here in order to be able to call FU service using GFORM test-only proxy endpoints.
    */
  private lazy val headers = Seq(`Csrf-Token: nocheck`)

}
