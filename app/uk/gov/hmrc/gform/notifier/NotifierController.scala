/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.notifier

import cats.instances.future._
import play.api.mvc.ControllerComponents
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierConfirmationCode, NotifierEmailAddress, NotifierTemplateId }

class NotifierController(
  controllerComponents: ControllerComponents,
  notifierAlgebra: NotifierAlgebra[FOpt],
  adjudicatorsEmailTemplateId: NotifierTemplateId)(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def sendEmail(notifierEmailAddress: NotifierEmailAddress) = Action.async(parse.json[NotifierConfirmationCode]) {
    implicit request =>
      val notifierEmail: NotifierEmail =
        NotifierEmail(
          adjudicatorsEmailTemplateId,
          notifierEmailAddress,
          Map("confirmation code" -> request.body.code),
          NotifierEmailReference("")
        )
      notifierAlgebra.email(notifierEmail).map(_ => NoContent).toFuture
  }
}
