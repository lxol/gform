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

import uk.gov.hmrc.gform.Spec

class SubmissionRefSpec extends Spec {

  "SubmissionRef.createSubmissionRef" should "generate submissionRef" in {
    val rnd = new scala.util.Random(12)
    val submissionRef = SubmissionRef(envelopeId)

    submissionRef.value should be("6FJX-HVQL-U4FD")

  }
}
