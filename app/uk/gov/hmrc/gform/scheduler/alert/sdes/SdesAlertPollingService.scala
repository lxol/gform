/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.alert.sdes

import akka.actor.ActorSystem
import uk.gov.hmrc.gform.scheduler.alert.AlertPoolingService
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesWorkItem

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class SdesAlertPollingService(
  actorSystem: ActorSystem,
  pollerInitialDelay: FiniteDuration,
  pollerInterval: FiniteDuration,
  pollerToggle: Boolean,
  sdesAlertService: SdesAlertService
)(implicit ec: ExecutionContext)
    extends AlertPoolingService[SdesWorkItem](actorSystem, sdesAlertService, pollerToggle) {

  override def name: String = "SdesAlertPollingService"

  override def initialDelay: FiniteDuration = pollerInitialDelay

  override def interval: FiniteDuration = pollerInterval
}
