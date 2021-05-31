package code.api.MxOpenBanking

import code.api.util.CustomJsonFormats
import com.openbankproject.commons.model.Bank
import net.liftweb.json.JValue

import scala.collection.immutable.List
import com.openbankproject.commons.model._

case class JvalueCaseClass(jvalueToCaseclass: JValue)

case class MetaBis(
  LastUpdated: String = "2021-05-25T17:59:24.297Z",
  TotalResults: Double=0,
  Agreement: String ="To be confirmed",
  License: String="To be confirmed",
  TermsOfUse: String="To be confirmed"
)
case class OtherAccessibility(
  Code: String,
  Description: String,
  Name: String
)
case class MxBranchV100(
  Identification: String
)
case class Site(
  Identification: String,
  Name: String
)
case class GeographicCoordinates(
  Latitude: String,
  Longitude: String
)
case class GeoLocation(
  GeographicCoordinates: GeographicCoordinates
)
case class PostalAddress(
  AddressLine: String,
  BuildingNumber: String,
  StreetName: String,
  TownName: String,
  CountrySubDivision: List[String],
  Country: String,
  PostCode: String,
  GeoLocation: GeoLocation
)
case class Location(
  LocationCategory: List[String],
  OtherLocationCategory: List[OtherAccessibility],
  Site: Site,
  PostalAddress: PostalAddress
)
case class FeeSurcharges(
  CashWithdrawalNational: String,
  CashWithdrawalInternational: String,
  BalanceInquiry: String
)
case class MxATMV100(
  Identification: String,
  SupportedLanguages: Option[List[String]],
  ATMServices: List[String],
  Accessibility: List[String],
  Access24HoursIndicator: Boolean,
  SupportedCurrencies: List[String],
  MinimumPossibleAmount: String,
  Note: List[String],
  OtherAccessibility: List[OtherAccessibility],
  OtherATMServices: List[OtherAccessibility],
  Branch: MxBranchV100,
  Location: Location,
  FeeSurcharges: FeeSurcharges
)
case class Brand(
  BrandName: String,
  ATM: List[MxATMV100]
)
case class Data(
  Brand: List[Brand]
)
case class GetAtmsResponseJson(
  meta: MetaBis,
  data: List[Data]
)
object JSONFactory_MX_OPEN_FINANCE_0_0_1 extends CustomJsonFormats {
   def createGetAtmsResponse (banks: List[Bank], atms: List[AtmT]) :GetAtmsResponseJson = {
     val brandList = banks
       //first filter out the banks without the atms
       .filter(bank =>atms.map(_.bankId).contains(bank.bankId))
       .map(bank => {
       val bankAtms = atms.filter(_.bankId== bank.bankId)
       Brand(
         BrandName = bank.fullName,
         ATM = bankAtms.map{ bankAtm =>
           MxATMV100(
             Identification = bankAtm.atmId.value,
             SupportedLanguages = Some(List("es","en")),//TODO provide dummy data firstly, need to prepare obp data and map it.
             ATMServices = List("ATBA","ATBP"),  //TODO provide dummy data firstly, need to prepare obp data and map it. 
             Accessibility = List("ATAD","ATAC"), //TODO provide dummy data firstly, need to prepare obp data and map it. 
             Access24HoursIndicator = true,//TODO 6 
             SupportedCurrencies = List("USD","MXN"), //TODO provide dummy data firstly, need to prepare obp data and map it.
             MinimumPossibleAmount = "5", //TODO 5 Add field ATM.minimum_withdrawal and add OBP PUT endpoint to set /atms/minimum-withdrawal
             Note = List("String1","Sting2"),//TODO provide dummy data firstly, need to prepare obp data and map it. 
             OtherAccessibility = List(OtherAccessibility("string","string","string")), //TODO8 Add table atm_other_accessibility_features with atm_id and the fields below and add OBP PUT endpoint to set /atms/ATM_ID/other-accessibility-features
             OtherATMServices = List(OtherAccessibility("string","string","string")), //TODO 9 Add table atm_other_services with atm_id and the fields below and add OBP PUT endpoint to set /atms/ATM_ID/other-services              
             Branch = MxBranchV100("N/A"), //TODO 10 Add field branch_identification String
             Location = Location(
               LocationCategory = List("ATBI","ATBE"), //TODO 11 # Add field location_categories (comma separated list)
               OtherLocationCategory = List(OtherAccessibility("string","string","string")), //TODO 12 Add Table atm_other_location_category with atm_id and the following fields and a PUT endpoint /atms/ATM_ID/other-location-categories
               Site = Site(
                 Identification = "String",
                 Name= "String"
               ),//TODO 13 # Add fields site_identification and site_name
               PostalAddress = PostalAddress(
                 AddressLine= bankAtm.address.line1,
                 BuildingNumber= bankAtm.address.line2,
                 StreetName= bankAtm.address.line3,
                 TownName= bankAtm.address.city,
                 CountrySubDivision = List(bankAtm.address.state), 
                 Country = bankAtm.address.county.getOrElse(""),
                 PostCode= bankAtm.address.postCode,
                 GeoLocation = GeoLocation(
                   GeographicCoordinates(
                     bankAtm.location.latitude.toString,
                     bankAtm.location.longitude.toString
                     
                   )
                 )
               )
             ),
             FeeSurcharges = FeeSurcharges(
               CashWithdrawalNational = "String",
               CashWithdrawalInternational = "String",
               BalanceInquiry = "String") //TODO 13 # add fields cash_withdrawal_national_fee, cash_withdrawal_international_fee, balance_enquiry_fee
           )
         }
       )
     }
     )
     GetAtmsResponseJson(
       meta = MetaBis(),
       data = List(Data(brandList))
     )
   }
}
