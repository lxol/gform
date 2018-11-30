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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.Logger
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HttpResponse

class Helper(config: FUConfig, timeProvider: TimeProvider) {

  def createEnvelopeRequestBody(formTemplateId: FormTemplateId): JsObject =
    Json.obj(
      "constraints" -> Json.obj(
        "contentTypes"   -> contentTypesJson,
        "maxItems"       -> config.maxItems,
        "maxSize"        -> config.maxSize,
        "maxSizePerItem" -> config.maxSizePerItem),
      "callbackUrl" -> "someCallback",
      "expiryDate"  -> s"$envelopeExpiryDate",
      "metadata"    -> Json.obj("application" -> "gform", "formTemplateId" -> s"${formTemplateId.value}")
    )

  /**
    * There must be Location header. If not this is exceptional situation!
    */
  def extractEnvelopId(resp: HttpResponse): EnvelopeId = resp.header(LOCATION) match {
    case Some(EnvelopeIdExtractor(envelopeId)) => EnvelopeId(envelopeId)
    case Some(location)                        => throw new SpoiltLocationHeader(location)
    case _                                     => throw new SpoiltLocationHeader(s"Header $LOCATION not found")
  }

  private lazy val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  private def envelopeExpiryDate = {
    val time: LocalDateTime = timeProvider.localDateTime()
    val expiryDays = config.expiryDays.toLong
    val timePlus: LocalDateTime = time.plusDays(expiryDays)
    val exp = timePlus.format(formatter)
    Logger.info(s"Envelope expiry current time ${time
      .format(formatter)} expiry days $expiryDays and calculated expiry time ${timePlus.format(formatter)}")

    // Time bug workaround tryouts

    val timePlus2 = time.plusHours(expiryDays * 24)

    Logger.info(s"time.plusHours(expiryDays * 24) ${timePlus2.format(formatter)}")

    val timePlus3 = time.plusYears(1).minusDays(365 - expiryDays) // This will yield one day more before and during a leap year

    Logger.info(s"time.plusYears(1).minusDays(365 - expiryDays) ${timePlus3.format(formatter)}")

    val timePlus4 = time.plusDays(expiryDays * 2).minusDays(expiryDays) // this might go wrong at the start of November next year

    Logger.info(s"time.plusDays(expiryDays * 2).minusDays(expiryDays) ${timePlus4.format(formatter)}")

    val timePlus5 = time.minusYears(1).plusDays(expiryDays).plusYears(1) // this should work until 2020

    Logger.info(s"time.minusYears(1).plusDays(expiryDays).plusYear(1) ${timePlus5.format(formatter)}")

    val timePlus6 = time.plusHours(expiryDays * 24 - 1).plusHours(1)

    Logger.info(s"time.plusHours(expiryDays * 24 - 1).plusHours(1) ${timePlus6.format(formatter)}")

    Logger.info("Setting fixed envelope expiry time as an emergency fix 2018-12-30T10:25:38Z")
    "2018-12-30T10:25:38Z" // TODO This is an emergency fix, only
  }
  private val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
  private lazy val contentTypesJson = Json.toJson(config.contentTypes)
}
