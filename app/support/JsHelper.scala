package support

import java.net.URLEncoder
import play.api.libs.json._
import Bindables._
import cube._
import models._

object JsHelper {

  /** Pendant to window.fudim.point.parse */
  def serializePoint(p: Point): String = {
    val json = Json.toJson(p.coordinates.map(DimensionRepo.serializeCoordinate).toMap)
    URLEncoder.encode(json.toString, "UTF-8")
  }

}