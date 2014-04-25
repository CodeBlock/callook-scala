package me.elrod.callook

import argonaut._, Argonaut._
import java.io.{ DataOutputStream, InputStream }
import java.net.{ HttpURLConnection, URL, URLEncoder }
import scala.io.{ Codec, Source }
import scalaz._, Scalaz._
import scalaz.effect._
//import scalaz.stream._
//import scalaz.concurrent.Task

sealed trait LookupStatus
case object Valid extends LookupStatus
case object Invalid extends LookupStatus
case object Updating extends LookupStatus

sealed trait LicenseType
case object Club extends LicenseType
case object Military extends LicenseType
case object Races extends LicenseType
case object Recreation extends LicenseType
case object Person extends LicenseType
sealed trait LicenseTypeInstances {
  implicit val LicenseTypeShow: Show[LicenseType] = new Show[LicenseType] {
    override def shows(l: LicenseType) =
      l match {
        case Club       => "club"
        case Military   => "military"
        case Races      => "races"
        case Recreation => "recreation"
        case Person     => "person"
        case _          => "unknown"
      }
  }
}
object LicenseType extends LicenseTypeInstances

sealed trait OperatorClass
case object Novice extends OperatorClass
case object Technician extends OperatorClass
case object TechnicianPlus extends OperatorClass
case object General extends OperatorClass
case object Advanced extends OperatorClass
case object Extra extends OperatorClass
sealed trait OperatorClassInstances {
  implicit val OperatorClassShow: Show[OperatorClass] = new Show[OperatorClass] {
    override def shows(l: OperatorClass) =
      l match {
        case Novice         => "novice"
        case Technician     => "technician"
        case TechnicianPlus => "technician plus"
        case General        => "general"
        case Advanced       => "advanced"
        case Extra          => "extra"
      }
  }
}
object OperatorClass extends OperatorClassInstances

case class CallookResponse(
  status: String,
  licenseType: Option[LicenseType],
  current: CallsignClass,
  previous: CallsignClass,
  trustee: Trustee,
  name: String,
  address: Address,
  location: Location,
  otherInfo: OtherInfo
)
case class CallsignClass(callsign: String, operatorClass: Option[OperatorClass])
case class Trustee(callsign: String, name: String)
case class Address(line1: String, line2: String, line3: String)
case class Location(latitude: String, longitude: String, gridsquare: String)
case class OtherInfo(grantDate: String, expiryDate: String, lastActionDate: String, frn: String, ulsUrl: String)

object Callook {
  import LicenseType._
  import OperatorClass._

  implicit def LicenseTypeOptionEncodeJson: EncodeJson[Option[LicenseType]] =
    EncodeJson((x: Option[LicenseType]) =>
      x match {
        case Some(t) => Json("type" -> jString(t.shows.toUpperCase))
        case None => jNull
      })

  implicit def LicenseTypeEncodeJson: EncodeJson[LicenseType] =
    EncodeJson((x: LicenseType) => jString(x.shows.toUpperCase))

  implicit def CallsignClassEncodeJson: EncodeJson[CallsignClass] =
    EncodeJson((x: CallsignClass) =>
      Json(
        "callsign" -> jString(x.callsign),
        "operClass" -> x.operatorClass.map(v => jString(v.shows.toUpperCase)).getOrElse(jNull)
      ))

  implicit def CallookCodecJson: CodecJson[CallookResponse] =
    casecodec9(CallookResponse.apply, CallookResponse.unapply)("status", "type", "current", "previous", "trustee", "name", "address", "location", "otherInfo")

  implicit def CallsignClassCodecJson: DecodeJson[CallsignClass] =
    DecodeJson(c => for {
      callsign <- (c --\ "callsign").as[String]
      opClassStr <- (c --\ "operClass").as[String]
    } yield CallsignClass(callsign, strToOpClass(opClassStr)))

  def strToOpClass(c: String): Option[OperatorClass] =
    c match {
      case "NOVICE" => Some(Novice)
      case "TECHNICIAN" => Some(Technician)
      case "TECHNICIAN PLUS" => Some(TechnicianPlus)
      case "GENERAL" => Some(General)
      case "ADVANCED" => Some(Advanced)
      case "EXTRA" => Some(Extra)
      case _ => None
    }

  implicit def LicenseTypeCodecJson: DecodeJson[Option[LicenseType]] =
    DecodeJson(c => for {
      licenseType <- c.as[String]
    } yield strToLicenseType(licenseType))

  def strToLicenseType(c: String): Option[LicenseType] =
    c match {
      case "CLUB" => Some(Club)
      case "MILITARY" => Some(Military)
      case "RACES" => Some(Races)
      case "RECREATION" => Some(Recreation)
      case "PERSON" => Some(Person)
      case _ => None
    }

  implicit def TrusteeCodecJson: CodecJson[Trustee] =
    casecodec2(Trustee.apply, Trustee.unapply)("callsign", "name")

  implicit def AddressCodecJson: CodecJson[Address] =
    casecodec3(Address.apply, Address.unapply)("line1", "line2", "attn")

  implicit def LocationCodecJson: CodecJson[Location] =
    casecodec3(Location.apply, Location.unapply)("latitude", "longitude", "gridsquare")

  implicit def OtherInfoCodecJson: CodecJson[OtherInfo] =
    casecodec5(OtherInfo.apply, OtherInfo.unapply)("grantDate", "expiryDate", "lastActionDate", "frn", "ulsUrl")

/*  implicit def CallookDecodeJson: DecodeJson[CallookResponse] =
    DecodeJson(c => for {
      startChar <- c.\\.as[Int]
      length    <- (c.\\ :->- 1).as[Int]
      metadata  <- (c.\\ :->- 2).as[List[(String, String)]]
    } yield Token(startChar, length, metadata))

  implicit def ResponseDecodeJson: DecodeJson[InterpretResponse] =
    DecodeJson(c => {
      val first = c.\\
      val second = (first =\ 0) :->- 1
      val tokens = second.\\ :->- 2
      for {
        responseType <- (first =\ 0).as[String]
        status       <- second.\\.as[String]
        result       <- (second.\\ :->- 1).as[String]
        tokens       <- (second.\\ :->- 2).as[Option[List[Token]]]
      } yield InterpretResponse(responseType, status, result, tokens)
    })*/

  def callsignIO(callsign: String): IO[InputStream] = IO {
    val endpoint = new URL("http://callook.info/" + callsign + "/json")
    val connection: HttpURLConnection = endpoint.openConnection.asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setDoOutput(true)
    connection.setRequestProperty("Content-Type", "application/json")
    connection.getInputStream
  }

  def toUtf8String(i: InputStream): String =
    Source.fromInputStream(i)(Codec.UTF8).mkString

  def toJson: String => String \/ CallookResponse = _.decodeEither[CallookResponse]
}

