/**
Open Bank Project - API
Copyright (C) 2011-2019, TESOBE GmbH.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com
TESOBE GmbH.
Osloer Strasse 16/17
Berlin 13359, Germany

This product includes software developed at
TESOBE (http://www.tesobe.com/)

  */
package code.snippet

import code.api.RequestHeader
import code.api.berlin.group.v1_3.JSONFactory_BERLIN_GROUP_1_3.{GetConsentResponseJson, createGetConsentResponseJson}
import code.api.util.{ConsentJWT, CustomJsonFormats, JwtUtil}
import code.api.v3_1_0.APIMethods310
import code.api.v5_0_0.APIMethods500
import code.api.v5_1_0.APIMethods510
import code.consent.{ConsentStatus, Consents, MappedConsent}
import code.consumer.Consumers
import code.model.dataAccess.AuthUser
import code.util.Helper.{MdcLoggable, ObpS}
import net.liftweb.common.{Box, Failure, Full}
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{RequestVar, S, SHtml, SessionVar}
import net.liftweb.json.{Formats, parse}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import scala.collection.immutable

class BerlinGroupConsent extends MdcLoggable with RestHelper with APIMethods510 with APIMethods500 with APIMethods310 {
  protected implicit override def formats: Formats = CustomJsonFormats.formats

  private object otpValue extends SessionVar("123")
  private object redirectUriValue extends SessionVar("")

  def confirmBerlinGroupConsentRequest: CssSel = {
    callGetConsentByConsentId() match {
      case Full(consent) =>
        otpValue.set(consent.challenge)
        val json: GetConsentResponseJson = createGetConsentResponseJson(consent)
        val consumer = Consumers.consumers.vend.getConsumerByConsumerId(consent.consumerId)
        val consentJwt: Box[ConsentJWT] = JwtUtil.getSignedPayloadAsJson(consent.jsonWebToken).map(parse(_)
          .extract[ConsentJWT])
        val tppRedirectUri: immutable.Seq[String] = consentJwt.map{ h =>
          h.request_headers.filter(h => h.name == RequestHeader.`TPP-Redirect-URL`)
        }.getOrElse(Nil).map((_.values.mkString("")))
        val consumerRedirectUri: Option[String] = consumer.map(_.redirectURL.get).toOption
        val uri: String = tppRedirectUri.headOption.orElse(consumerRedirectUri).getOrElse("https://not.defined.com")
        redirectUriValue.set(uri)
        val formText =
          s"""I, ${AuthUser.currentUser.map(_.firstName.get).getOrElse("")} ${AuthUser.currentUser.map(_.lastName.get).getOrElse("")}, consent to the service provider ${consumer.map(_.name.get).getOrElse("")} making actions on my behalf.
          |
          |This consent must respects the following actions:
          |
          |   1) Can read accounts: ${json.access.accounts.getOrElse(Nil).flatMap(_.iban).mkString(", ")}
          |   2) Can read balances: ${json.access.balances.getOrElse(Nil).flatMap(_.iban).mkString(", ")}
          |   3) Can read transactions: ${json.access.transactions.getOrElse(Nil).flatMap(_.iban).mkString(", ")}
          |
          |This consent will end on date ${json.validUntil}.
          |
          |I understand that I can revoke this consent at any time.
          |""".stripMargin


            "#confirm-bg-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
            "#confirm-bg-consent-request-form-text *" #> s"""$formText""" &
            "#confirm-bg-consent-request-confirm-submit-button" #> SHtml.onSubmitUnit(confirmConsentRequestProcess) &
            "#confirm-bg-consent-request-deny-submit-button" #> SHtml.onSubmitUnit(denyConsentRequestProcess)
      case everythingElse =>
        S.error(everythingElse.toString)
        "#confirm-bg-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
          "type=submit" #> ""
    }
  }

  private def callGetConsentByConsentId(): Box[MappedConsent] = {
    val requestParam = List(
      ObpS.param("CONSENT_ID"),
    )
    if (requestParam.count(_.isDefined) < requestParam.size) {
      Failure("Parameter CONSENT_ID is missing, please set it in the URL")
    } else {
      val consentId = ObpS.param("CONSENT_ID") openOr ("")
      Consents.consentProvider.vend.getConsentByConsentId(consentId)
    }
  }

  private def confirmConsentRequestProcess() = {
    val consentId = ObpS.param("CONSENT_ID") openOr ("")
    S.redirectTo(
      s"/confirm-bg-consent-request-sca?CONSENT_ID=${consentId}"
    )
  }
  private def denyConsentRequestProcess() = {
    val consentId = ObpS.param("CONSENT_ID") openOr ("")
    Consents.consentProvider.vend.updateConsentStatus(consentId, ConsentStatus.rejected)
    S.redirectTo(
      s"$redirectUriValue?CONSENT_ID=${consentId}"
    )
  }
  private def confirmConsentRequestProcessSca() = {
    val consentId = ObpS.param("CONSENT_ID") openOr ("")
    Consents.consentProvider.vend.getConsentByConsentId(consentId) match {
      case Full(consent) if otpValue.is == consent.challenge =>
        Consents.consentProvider.vend.updateConsentStatus(consentId, ConsentStatus.valid)
        S.redirectTo(
          s"$redirectUriValue?CONSENT_ID=${consentId}"
        )
      case _ =>
        S.error("Wrong OTP value")
    }
  }


  def confirmBgConsentRequest: CssSel = {
    "#otp-value" #> SHtml.text(otpValue, otpValue(_)) &
      "type=submit" #> SHtml.onSubmitUnit(confirmConsentRequestProcessSca)
  }
  
}
