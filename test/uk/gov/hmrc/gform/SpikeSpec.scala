package uk.gov.hmrc.gform

import com.softwaremill.sttp._
import pact.uk.gov.hmrc.gform.StubServer

import scala.io.Source
import scala.util.Random

class SpikeSpec extends Spec with StubServer {

  it should "blah" in {
    val templateId = Random.nextString(6)

    Source.fromURL("http://localhost:9199/ping/ping").getLines.mkString shouldBe ""
    val addUri = Uri("localhost", 9199, List("gform", "formtemplates"))

    val findUri = Uri("localhost", 9195, List("submissions", templateId))

    val addTemplateRequest = sttp.headers("Content-Type" -> "application/json").body(body(templateId)).post(addUri)
    val retrieveTemplateRequest = sttp.get(findUri)

    implicit val backend = HttpURLConnectionBackend()
    val addResponse = addTemplateRequest.send()
    val findResponse = retrieveTemplateRequest.send()

    addResponse.code shouldBe StatusCodes.NoContent

    val templateExist = sttp.get(Uri(s"localhost", 9199, List("gform", "formtemplates", s"$templateId"))).send()
    templateExist.code shouldBe StatusCodes.Ok

    //TODO change port config so frontend knows how to talk to 9199
//    findResponse.code shouldBe StatusCodes.Accepted
  }

  val body: String => String = id =>
    s"""{
      |  "_id": "$id",
      |  "formName": "return periods test",
      |  "description": "testing return periods",
      |  "dmsSubmission": {
      |    "dmsFormId": "",
      |    "customerId": "$${auth.gg}",
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
      |          "idNumber": "$${textBox}",
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

}
