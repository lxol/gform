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

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class JsonParseTestGroup extends Spec {

  "A raw group" should "parse" in {

    val jsonStr =
      """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":1,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ],
        "presentationHint" : "collapseGroupUnderLabel"

      }
    """

    val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr))

    jsResult should beJsSuccess(
      FormComponent(
        FormComponentId("gid"),
        Group(
          List(FormComponent(
            FormComponentId("cid"),
            Choice(Radio, NonEmptyList.of(toLocalisedString("A"), toLocalisedString("B")), Vertical, List(), None),
            toLocalisedString("clabel"),
            None,
            None,
            None,
            true,
            true,
            true,
            derived = false,
            onlyShowOnSummary = false,
            None
          )),
          Horizontal,
          Some(5),
          Some(1),
          Some(toLocalisedString("repeatLabel")),
          Some(toLocalisedString("repeatAddAnotherText"))
        ),
        toLocalisedString("glabel"),
        None,
        None,
        None,
        true,
        true,
        true,
        derived = false,
        onlyShowOnSummary = false,
        None,
        Some(List(CollapseGroupUnderLabel))
      ))

  }

  "A raw group with repeats set to 0" should "parse the FormComponent with mandatory set to false" in {

    val jsonStr =
      """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":0,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ],
        "presentationHint" : "collapseGroupUnderLabel"

      }
    """

    val jsResult = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr))

    jsResult should beJsSuccess(
      FormComponent(
        FormComponentId("gid"),
        Group(
          List(FormComponent(
            FormComponentId("cid"),
            Choice(Radio, NonEmptyList.of(toLocalisedString("A"), toLocalisedString("B")), Vertical, List(), None),
            toLocalisedString("clabel"),
            None,
            None,
            None,
            mandatory = false,
            true,
            true,
            derived = false,
            false,
            None
          )),
          Horizontal,
          Some(5),
          Some(0),
          Some(toLocalisedString("repeatLabel")),
          Some(toLocalisedString("repeatAddAnotherText"))
        ),
        toLocalisedString("glabel"),
        None,
        None,
        None,
        true,
        true,
        true,
        derived = false,
        false,
        None,
        Some(List(CollapseGroupUnderLabel))
      ))
  }

  "A raw group" should "fail to parse if repeatsMin/Max has errors" in {

    val jsonStr =
      """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":6,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ],
        "presentationHint" : "collapseGroupUnderLabel"

      }
    """

    var jsr: JsResult[FormComponent] = null
    jsr = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr)); jsr should be(jsError)
    jsr = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr.replaceAll("6", """"A""""))); jsr should be(jsError)
    jsr = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr.replaceAll("6", "-1"))); jsr should be(jsError)
    jsr = implicitly[Reads[FormComponent]].reads(Json.parse(jsonStr.replaceAll("5", """"A""""))); jsr should be(jsError)
  }

}
