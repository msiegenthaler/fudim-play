package support

import models._
import play.api.libs.json._
import Bindables._
import java.net.URLEncoder

object JsHelper {

  /** Pendant to window.fudim.point.parse */
  def serializePoint(p: Point): String = {
    val json = Json.toJson(p.coordinates.map(Coordinate.serialize).toMap)
    URLEncoder.encode(json.toString, "UTF-8")
  }

}