/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.validation

import cats.Functor
import cats.syntax.functor._
import uk.gov.hmrc.gform.bank_account_reputation.BankAccountReputationAlgebra
import uk.gov.hmrc.gform.des.DesAlgebra
import uk.gov.hmrc.gform.sharedmodel.ServiceCallResponse
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.sharedmodel.Account
import uk.gov.hmrc.http.HeaderCarrier

class ValidationService[F[_]: Functor](des: DesAlgebra[F], bankAccountReputation: BankAccountReputationAlgebra[F]) {
  def desRegistration(
    utr: String,
    desRegistrationRequest: DesRegistrationRequest
  ): F[ServiceCallResponse[DesRegistrationResponse]] =
    des.lookupRegistration(utr, desRegistrationRequest)

  def bankAccountReputation(account: Account)(implicit hc: HeaderCarrier): F[Boolean] =
    bankAccountReputation.exists(account).map(_.accountNumberWithSortCodeIsValid)
}
