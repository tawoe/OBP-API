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

import code.api.util.APIUtil._
import code.api.util.ErrorMessages.InvalidJsonFormat
import code.api.util.{APIUtil, CustomJsonFormats, DateTimeUtil}
import code.api.v5_1_0.{APIMethods510, ConsentJsonV510}
import code.api.v5_0_0.{APIMethods500, ConsentJsonV500, ConsentRequestResponseJson}
import code.api.v3_1_0.{APIMethods310, ConsentChallengeJsonV310, ConsumerJsonV310}
import code.consent.ConsentStatus
import code.consumer.Consumers
import code.model.dataAccess.AuthUser
import code.util.Helper.{MdcLoggable, ObpS}
import net.liftweb.common.Full
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{GetRequest, PostRequest, RequestVar, S, SHtml, SessionVar}
import net.liftweb.json
import net.liftweb.json.Formats
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

class VrpConsentCreation extends MdcLoggable with RestHelper with APIMethods510 with APIMethods500 with APIMethods310 {
  protected implicit override def formats: Formats = CustomJsonFormats.formats

  private object otpValue extends RequestVar("123456")
  private object consentRequestIdValue extends SessionVar("")

  def confirmVrpConsentRequest = {
    getConsentRequest match {
      case Left(error) => {
        S.error(error._1)
        "#confirm-vrp-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
          "#confirm-vrp-consent-request-response-json *" #> s"""""" &
          "type=submit" #> ""
      }
      case Right(response) => {
        tryo {json.parse(response).extract[ConsentRequestResponseJson]} match {
          case Full(consentRequestResponseJson) =>
            val jsonAst = consentRequestResponseJson.payload
            val currency = (jsonAst \ "to_account" \ "limit" \ "currency").extract[String]
            val ttl: Long = (jsonAst \ "time_to_live").extract[Long]
            val consumer = Consumers.consumers.vend.getConsumerByConsumerId(consentRequestResponseJson.consumer_id)
            val formText =
              s"""I, ${AuthUser.currentUser.map(_.firstName.get).getOrElse("")} ${AuthUser.currentUser.map(_.lastName.get).getOrElse("")}, consent to the service provider ${consumer.map(_.name.get).getOrElse("")} making transfers on my behalf from my bank account number ${(jsonAst \ "from_account" \ "account_routing" \ "address").extract[String]}, to the beneficiary ${(jsonAst \ "to_account" \ "counterparty_name").extract[String]}, account number ${(jsonAst \ "to_account" \ "account_routing" \ "address").extract[String]} at bank code ${(jsonAst \ "to_account" \ "bank_routing" \ "address").extract[String]}.
              |
              |The transfers governed by this consent must respect the following rules:
              |
              |  1) The grand total amount will not exceed {limit ($currency), (${(jsonAst \ "to_account" \ "limit" \ "max_total_amount").extract[String]})}
              |  2) Any single amount will not exceed {limit ($currency), (${(jsonAst \ "to_account" \ "limit" \ "max_single_amount").extract[String]})}
              |  3) The maximum amount per month that can be transferred is {limit ($currency), (${(jsonAst \ "to_account" \ "limit" \ "max_monthly_amount").extract[String]})} over {limit (${(jsonAst \ "to_account" \ "limit" \ "max_number_of_monthly_transactions").extract[String]}) transactions.
              |  4) The maximum amount per year that can be transferred is {limit ($currency), (${(jsonAst \ "to_account" \ "limit" \ "max_yearly_amount").extract[String]})} over {limit (${(jsonAst \ "to_account" \ "limit" \ "max_number_of_yearly_transactions").extract[String]}) transactions.
              |
              |This consent will start on date {${(jsonAst \ "valid_from").extract[String]}} and be valid for ${DateTimeUtil.formatDuration(ttl)}.
              |
              |I understand that I can revoke this consent at any time.
              |""".stripMargin


            "#confirm-vrp-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
            "#confirm-vrp-consent-request-form-text *" #> s"""$formText""" &
            "#from_bank_routing_scheme [value]" #> s"${(jsonAst \ "from_account" \ "bank_routing" \ "scheme").extract[String]}" &
            "#from_bank_routing_address [value]" #> s"${(jsonAst \ "from_account" \ "bank_routing" \ "address").extract[String]}" &
            "#from_branch_routing_scheme [value]" #> s"${(jsonAst \ "from_account" \ "branch_routing" \ "scheme").extract[String]}" &
            "#from_branch_routing_address [value]" #> s"${(jsonAst \ "from_account" \ "branch_routing" \ "address").extract[String]}" &
            "#from_routing_scheme [value]" #> s"${(jsonAst \ "from_account" \ "account_routing" \ "scheme").extract[String]}" &
            "#from_routing_address [value]" #> s"${(jsonAst \ "from_account" \ "account_routing" \ "address").extract[String]}" &
            "#to_bank_routing_scheme [value]" #> s"${(jsonAst \ "to_account" \ "bank_routing" \ "scheme").extract[String]}" &
            "#to_bank_routing_address [value]" #> s"${(jsonAst \ "to_account" \ "bank_routing" \ "address").extract[String]}" &
            "#to_branch_routing_scheme [value]" #> s"${(jsonAst \ "to_account" \ "branch_routing" \ "scheme").extract[String]}" &
            "#to_branch_routing_address [value]" #> s"${(jsonAst \ "to_account" \ "branch_routing" \ "address").extract[String]}" &
            "#to_routing_scheme [value]" #> s"${(jsonAst \ "to_account" \ "account_routing" \ "scheme").extract[String]}" &
            "#to_routing_address [value]" #> s"${(jsonAst \ "to_account" \ "account_routing" \ "address").extract[String]}" &
            "#counterparty_name [value]" #> s"${(jsonAst \ "to_account" \ "counterparty_name").extract[String]}" &
            "#currency [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "currency").extract[String]}" &
            "#max_single_amount [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_single_amount").extract[String]}" &
            "#max_monthly_amount [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_monthly_amount").extract[String]}" &
            "#max_yearly_amount [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_yearly_amount").extract[String]}" &
            "#max_total_amount [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_total_amount").extract[String]}" &
            "#max_number_of_monthly_transactions [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_number_of_monthly_transactions").extract[String]}" &
            "#max_number_of_yearly_transactions [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_number_of_yearly_transactions").extract[String]}" &
            "#max_number_of_transactions [value]" #> s"${(jsonAst \ "to_account" \ "limit" \ "max_number_of_transactions").extract[String]}" &
            "#time_to_live_in_seconds [value]" #> s"${(jsonAst \ "time_to_live").extract[String]}" &
            "#valid_from [value]" #> s"${(jsonAst \ "valid_from").extract[String]}" &
            "#email [value]" #> s"${(jsonAst \ "email").extract[String]}" &
            "#phone_number [value]" #> s"${(jsonAst \ "phone_number").extract[String]}" &
              showHideElements &
            "#confirm-vrp-consent-request-confirm-submit-button" #> SHtml.onSubmitUnit(confirmConsentRequestProcess)
          case _ =>
            "#confirm-vrp-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
            "#confirm-vrp-consent-request-form-title *" #> s"Please confirm or deny the following consent request:" &
              "#confirm-vrp-consent-request-response-json *" #>
                s"""$InvalidJsonFormat The Json body should be the $ConsentRequestResponseJson. 
                   |Please check `Get Consent Request` endpoint separately! """.stripMargin &
              "type=submit" #> ""
        }
      }
    }
    
  }

  def showHideElements: CssSel = {
    if (ObpS.param("format").isEmpty) {
      "#confirm-vrp-consent-request-form-text-div [style]" #> "display:block" &
      "#confirm-vrp-consent-request-form-fields [style]" #> "display:none"
    } else if(ObpS.param("format").contains("1")) {
      "#confirm-vrp-consent-request-form-text-div [style]" #> "display:none" &
      "#confirm-vrp-consent-request-form-fields [style]" #> "display:block"
    } else if(ObpS.param("format").contains("2")) {
      "#confirm-vrp-consent-request-form-text-div [style]" #> "display:block" &
      "#confirm-vrp-consent-request-form-fields [style]" #> "display:none"
    }  else if(ObpS.param("format").contains("3")) {
      "#confirm-vrp-consent-request-form-text-div [style]" #> "display:block" &
      "#confirm-vrp-consent-request-form-fields [style]" #> "display:block"
    } else {
      "#confirm-vrp-consent-request-form-text-div [style]" #> "display:block" &
      "#confirm-vrp-consent-request-form-fields [style]" #> "display:none"
    }
  }
  
  def confirmVrpConsent = {
    "#otp-value" #> SHtml.textElem(otpValue) &
      "type=submit" #> SHtml.onSubmitUnit(confirmVrpConsentProcess)
  }
  
  private def confirmConsentRequestProcess() ={
    //1st: we need to call `Create Consent By CONSENT_REQUEST_ID (IMPLICIT)`, this will send OTP to account owner.
    callCreateConsentByConsentRequestIdImplicit match {
      case Left(error) => {
        S.error(error._1)
      }
      case Right(response) => {
        tryo {json.parse(response).extract[ConsentJsonV500]} match {
          case Full(consentJsonV500) =>
            //2nd: we need to redirect to confirm page to fill the OTP
            S.redirectTo(
              s"/confirm-vrp-consent?CONSENT_ID=${consentJsonV500.consent_id}"
            )
          case _ =>
            S.error(s"$InvalidJsonFormat The Json body should be the $ConsentJsonV500. " +
              s"Please check `Create Consent By CONSENT_REQUEST_ID (IMPLICIT) !")
        }
      }
    }
  }

  private def callAnswerConsentChallenge: Either[(String, Int), String] = {

    val requestParam = List(
      ObpS.param("CONSENT_ID")
    )

    if(requestParam.count(_.isDefined) < requestParam.size) {
      return Left(("There are one or many mandatory request parameter not present, please check request parameter: CONSENT_ID", 500))
    }

    val pathOfEndpoint = List(
      "banks",
      APIUtil.defaultBankId,//we do not need to get this from URL, it will be easier for the developer.
      "consents",
      ObpS.param("CONSENT_ID")openOr(""),
      "challenge"
    )

    val requestBody = s"""{"answer":"${otpValue.get}"}"""
    val authorisationsResult = callEndpoint(Implementations3_1_0.answerConsentChallenge, pathOfEndpoint, PostRequest, requestBody)

    authorisationsResult

  }
  
  private def callGetConsentByConsentId(consentId: String): Either[(String, Int), String] = {
    
    val pathOfEndpoint = List(
      "user",
      "current",
      "consents",
      consentId,
    )

    val authorisationsResult = callEndpoint(Implementations5_1_0.getConsentByConsentId, pathOfEndpoint, GetRequest)

    authorisationsResult
  }
  
  private def callGetConsumer(consumerId: String): Either[(String, Int), String] = {
    
    val pathOfEndpoint = List(
      "management",
      "consumers",
      consumerId,
    )

    val authorisationsResult = callEndpoint(Implementations5_1_0.getConsumer, pathOfEndpoint, GetRequest)

    authorisationsResult
  }
  
  private def callCreateConsentByConsentRequestIdImplicit: Either[(String, Int), String] = {

    val requestParam = List(
      ObpS.param("CONSENT_REQUEST_ID"),
    )
    if(requestParam.count(_.isDefined) < requestParam.size) {
      return Left(("Parameter CONSENT_REQUEST_ID is missing, please set it in the URL", 500))
    }
    
    val pathOfEndpoint = List(
      "consumer",
      "consent-requests",
      ObpS.param("CONSENT_REQUEST_ID")openOr(""),
      "IMPLICIT",
      "consents",
    )

    val requestBody = s"""{}"""
    val authorisationsResult = callEndpoint(Implementations5_0_0.createConsentByConsentRequestIdImplicit, pathOfEndpoint, PostRequest, requestBody)

    authorisationsResult
  }
  
  private def confirmVrpConsentProcess() ={
    //1st: we need to answer challenge and create the consent properly.
    callAnswerConsentChallenge match {
      case Left(error) => S.error(error._1)
      case Right(response) => {
        tryo {json.parse(response).extract[ConsentChallengeJsonV310]} match {
          case Full(consentChallengeJsonV310) if (consentChallengeJsonV310.status.equals(ConsentStatus.ACCEPTED.toString)) =>
            //2nd: we need to call getConsent by consentId --> get the consumerId
            callGetConsentByConsentId(consentChallengeJsonV310.consent_id)  match {
              case Left(error) => S.error(error._1)
              case Right(response) => {
                tryo {json.parse(response).extract[ConsentJsonV510]} match {
                  case Full(consentJsonV510) =>
                    //3rd: get consumer by consumerId
                    callGetConsumer(consentJsonV510.consumer_id)  match {
                      case Left(error) => S.error(error._1)
                      case Right(response) => {
                        tryo {json.parse(response).extract[ConsumerJsonV310]} match {
                          case Full(consumerJsonV310) =>
                            //4th: get the redirect url.
                            val redirectURL = consumerJsonV310.redirect_url.trim
                            S.redirectTo(s"$redirectURL?CONSENT_REQUEST_ID=${consentJsonV510.consent_request_id.getOrElse("")}&status=${consentJsonV510.status}")
                          case _ =>
                            S.error(s"$InvalidJsonFormat The Json body should be the $ConsumerJsonV310. " +
                              s"Please check `Get Consumer` !")
                        }
                      }
                    }
                    
                  case _ =>
                    S.error(s"$InvalidJsonFormat The Json body should be the $ConsentJsonV510. " +
                      s"Please check `Get Consent By Consent Id` !")
                }
              }
            }
          case Full(consentChallengeJsonV310) =>
            S.error(s"Current SCA status is ${consentChallengeJsonV310.status}. Please double check OTP value.")
          case _ => S.error(s"$InvalidJsonFormat The Json body should be the $ConsentChallengeJsonV310. " +
            s"Please check `Answer Consent Challenge` ! ")
        }
      }
    }
  }

  private def getConsentRequest: Either[(String, Int), String] = {

    val requestParam = List(
      ObpS.param("CONSENT_REQUEST_ID"),
    )

    if(requestParam.count(_.isDefined) < requestParam.size) {
      return Left(("Parameter CONSENT_REQUEST_ID is missing, please set it in the URL", 500))
    }

    val consentRequestId = ObpS.param("CONSENT_REQUEST_ID")openOr("")
    consentRequestIdValue.set(consentRequestId)

    val pathOfEndpoint = List(
      "consumer",
      "consent-requests",
      consentRequestId
    )

    val authorisationsResult = callEndpoint(Implementations5_0_0.getConsentRequest, pathOfEndpoint, GetRequest)

    authorisationsResult
  }
  
}
