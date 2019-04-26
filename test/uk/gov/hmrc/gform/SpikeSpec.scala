package uk.gov.hmrc.gform

import com.softwaremill.sttp._
import pact.uk.gov.hmrc.gform.StubServer
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{AcknowledgementSection, Constant, DeclarationSection, FormTemplate, FormTemplateId, HmrcSimpleModule, TextExpression}

import scala.io.Source

class SpikeSpec extends Spec with StubServer {

  it should "blah" in {

    Source.fromURL("http://localhost:9199/ping/ping").getLines.mkString shouldBe ""
    val uri = Uri("localhost", 9199, List("gform", "formtemplates"))
    val request = sttp.headers("Content-Type" -> "application/json").body(body).post(uri)

    implicit val backend = HttpURLConnectionBackend()
    val response = request.send()

    response.code shouldBe StatusCodes.NoContent
  }

  val body =
    """{
      |  "_id": "TST456",
      |  "formName": "return periods test",
      |  "description": "testing return periods",
      |  "dmsSubmission": {
      |    "dmsFormId": "",
      |    "customerId": "${auth.gg}",
      |    "classificationType": "",
      |    "businessArea": ""
      |  },
      |  "authConfig": {
      |    "authModule": "hmrc"
      |  },
      |  "emailTemplateId": "",
      |  "submitSuccessUrl": "",
      |  "submitErrorUrl": "",
      |  "sections": [
      |    {
      |      "title": "Text box",
      |      "fields": [
      |        {
      |          "id": "textBox",
      |      "type": "text",
      |          "label": "Some Text",
      |          "mandatory": "true"
      |        }
      |      ]
      |    },
      |    {
      |      "title": "Tax Period",
      |      "fields": [
      |        {
      |          "id": "returnPeriod",
      |          "type": "hmrcTaxPeriod",
      |          "label": "",
      |      "idType": "eeits",
      |          "idNumber": "${textBox}",
      |          "regimeType": "AGL"
      |        }
      |      ]
      |    }
      |  ],
      |  "declarationSection": {
      |    "title": "",
      |    "fields": []
      |  },
      |  "acknowledgementSection": {
      |    "title": "",
      |    "fields": []
      |  }
      |}""".stripMargin



  def persistAFormTemplate(formTemplateId: FormTemplateId) = {
    val akn = AcknowledgementSection("Mr", None, None, Nil)
    val declaration = DeclarationSection("Mr", None, None, Nil)
    val submission = DmsSubmission("id", TextExpression(Constant("costant")), "classification", "BA")
    val raw = FormTemplate(
      formTemplateId,
      "name",
      "description",
      None,
      None,
      None,
      None,
      submission,
      None,
      HmrcSimpleModule,
      "classification",
      None,
      "",
      "business",
      None,
      Nil,
      akn,
      declaration,
      None
    )

    stubbedModule.module.formTemplateModule.formTemplateService.verifyAndSave(raw)
  }

}
