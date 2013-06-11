package support

import models._
import play.api.libs.json._
import Bindables._
import java.net.URLEncoder

object JsHelper {

  /** Pendant to window.fudim.point.parse */
  def serializePoint(p: Point): String = {
    val json = Json.toJson(p.values.map(e => (e._1.name, e._2)))
    URLEncoder.encode(json.toString, "UTF-8")
  }
  
}