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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, ExposedConfig }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

class ExposedConfigSpec extends support.ITSpec {

  "ExposedConfig" in eventually {

    gformConnector.getExposedConfig.futureValue shouldBe ExposedConfig(
      formMaxAttachmentSizeMB = 10,
      formExpiryDays = 28,
      List(
        ContentType.`application/pdf`,
        ContentType.`image/jpeg`,
        ContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
        ContentType(".xlsx"),
        ContentType("application/vnd.oasis.opendocument.spreadsheet"),
        ContentType(".ods"),
        ContentType("application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
        ContentType(".docx"),
        ContentType("application/vnd.oasis.opendocument.text"),
        ContentType(".odt"),
        ContentType("application/vnd.openxmlformats-officedocument.presentationml.presentation"),
        ContentType(".pptx"),
        ContentType("application/vnd.oasis.opendocument.presentation"),
        ContentType(".odp")
      )
    )
  }
}
