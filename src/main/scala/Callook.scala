package me.elrod.callook

import argonaut._, Argonaut._
import java.io.{ DataOutputStream, InputStream }
import java.net.{ HttpURLConnection, URL, URLEncoder }
import scala.io.{ Codec, Source }
import scalaz._, Scalaz._
import scalaz.effect._
//import scalaz.stream._
//import scalaz.concurrent.Task

case class CallookResponse(
  status: String,
  licenseType: String,
  current: CallsignClass,
  previous: CallsignClass,
  trustee: Trustee,
  name: String,
  address: Address,
  location: Location,
  otherInfo: OtherInfo
)
case class CallsignClass(callsign: String, operatorClass: String)
case class Trustee(callsign: String, name: String)
case class Address(line1: String, line2: String, line3: String)
case class Location(latitude: String, longitude: String, gridsquare: String)
case class OtherInfo(grantDate: String, expiryDate: String, lastActionDate: String, frn: String, ulsUrl: String)

object Callook {
  implicit def CallookCodecJson: CodecJson[CallookResponse] =
    casecodec9(CallookResponse.apply, CallookResponse.unapply)("status", "type", "current", "previous", "trustee", "name", "address", "location", "otherInfo")

  implicit def CallsignClassCodecJson: CodecJson[CallsignClass] =
    casecodec2(CallsignClass.apply, CallsignClass.unapply)("callsign", "operClass")

  implicit def TrusteeCodecJson: CodecJson[Trustee] =
    casecodec2(Trustee.apply, Trustee.unapply)("callsign", "name")

  implicit def AddressCodecJson: CodecJson[Address] =
    casecodec3(Address.apply, Address.unapply)("line1", "line2", "attn")

  implicit def LocationCodecJson: CodecJson[Location] =
    casecodec3(Location.apply, Location.unapply)("latitude", "longitude", "gridsquare")

  implicit def OtherInfoCodecJson: CodecJson[OtherInfo] =
    casecodec5(OtherInfo.apply, OtherInfo.unapply)("grantDate", "expiryDate", "lastActionDate", "frn", "ulsUrl")

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

