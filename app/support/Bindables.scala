package support

import util.control.Exception._
import play.api.mvc.QueryStringBindable
import java.net.{ URLEncoder, URLDecoder }
import models._

object Bindables {
  implicit object PointQueryStringBindable extends QueryStringBindable[Point] {
    override def bind(key: String, params: Map[String, Seq[String]]) = {
      try {
        val prefix = key + "."
        val values = params.filter(_._1.startsWith(prefix)).map(v ⇒ (v._1.drop(prefix.length), v._2)).filterNot(_._1.isEmpty).
          flatMap(v ⇒ v._2.map((v._1, _))).map(v ⇒ (dec(v._1), dec(v._2))).
          flatMap(v ⇒ Coordinate.parse(v))
        val point = values.foldLeft(Point.empty)(_ + _)
        Some(Right(point))
      } catch {
        case r: RuntimeException ⇒ Some(Left(r.toString))
      }
    }

    override def unbind(key: String, value: Point) = {
      value.coordinates.map(Coordinate.serialize).map(e ⇒ (enc(e._1), enc(e._2))).
        map(e ⇒ s"$key.${e._1}=${e._2}").mkString("&")
    }

    override def javascriptUnbind: String = "window.fudim.point.unbind"

    private def enc(s: String) = URLEncoder.encode(s, "UTF-8")
    private def dec(s: String) = URLDecoder.decode(s, "UTF-8")
  }

}