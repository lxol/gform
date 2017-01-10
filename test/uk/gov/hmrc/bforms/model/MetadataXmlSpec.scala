/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.model

import java.time.LocalDateTime
import org.scalatest._
import scala.xml.Elem

class MetadataXmlSpec extends FlatSpec with Matchers with StreamlinedXml {
  "metadata.xml" should "be generated" in {

    val dmsMetaData = DmsMetaData(
      formId = "some-fomr-id",
      formNino = None,
      authNino = None,
      classificationType = "some-classification-type",
      businessArea = "some-business-area"
    )

    val submission = Submission(
      submittedDate = LocalDateTime.of(2012, 12, 3, 12, 45),
      submissionRef = "some-submission-ref",
      dmsMetaData = dmsMetaData,
      submissionMark = Some("submission-mark"),
      casKey = Some("some-cas-key")
    )

    val pdfSummary = PdfSummary(
      submissionRef = "some-submission-ref",
      numberOfPages = 10L,
      pdfContent = Array.empty[Byte]
    )

    val submissionAndPdf = SubmissionAndPdf(
      submission = submission,
      pdfSummary = pdfSummary
    )

    val expected =
      <documents xmlns="http://govtalk.gov.uk/hmrc/gis/content/1">
        <document>
          <header>
            <title>some-submission-ref</title>
            <format>pdf</format>
            <mime_type>application/pdf</mime_type>
            <store>true</store>
            <source>dfs</source>
            <target>DMS</target>
            <reconciliation_id> some-recocilliatin-id </reconciliation_id>
          </header>
          <metadata>
            <attribute>
              <attribute_name>hmrc_time_of_receipt</attribute_name>
              <attribute_type>time</attribute_type>
              <attribute_values>
                <attribute_value>03/12/2012 12:45:00</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>time_xml_created</attribute_name>
              <attribute_type>time</attribute_type>
              <attribute_values>
                <attribute_value>03/12/2012 12:45:00</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>submission_reference</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-submission-ref</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>form_id</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-fomr-id</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>number_pages</attribute_name>
              <attribute_type>integer</attribute_type>
              <attribute_values>
                <attribute_value>10</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>source</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>dfs</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>customer_id</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>???</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>submission_mark</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>submission-mark</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>cas_key</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-cas-key</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>classification_type</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-classification-type</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>business_area</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-business-area</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>attachment_count</attribute_name>
              <attribute_type>int</attribute_type>
              <attribute_values>
                <attribute_value>123</attribute_value>
              </attribute_values>
            </attribute>
          </metadata>
        </document>
      </documents>

    val metadataXml = MetadataXml.getXml("some-submission-ref", "some-recocilliatin-id", submissionAndPdf)

    metadataXml should equal(expected)(after being streamlined[Elem])

  }
}
