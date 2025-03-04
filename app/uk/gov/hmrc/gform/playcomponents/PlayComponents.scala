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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.ahc.AhcWSComponents
import play.api._

class PlayComponents(val context: ApplicationLoader.Context, val builtInComponents: BuiltInComponents) {

  val ahcWSComponents: AhcWSComponents = new AhcWSComponents {
    override def environment: Environment = context.environment
    override def configuration: Configuration = context.initialConfiguration
    override def applicationLifecycle: ApplicationLifecycle = context.lifecycle
    override def materializer: Materializer = builtInComponents.materializer
  }

}
