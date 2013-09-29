package support

import util.control.Exception._
import play.api.mvc.QueryStringBindable
import java.net.{URLEncoder, URLDecoder}
import cube._
import models._
import domain._

sealed trait PointDefinition extends (Set[Dimension] => Point) {
  def apply(domain: Domain): Point = apply(domain.dimensions)
  def apply(fact: Fact[_]): Point = apply(fact.dimensions)
  private[support] def raw: Iterable[(String, Long)]
}
object PointDefinition {
  implicit def apply(p: Point): PointDefinition = new PointDefinition {
    override def apply(ds: Set[Dimension]) = p
    override def raw = p.coordinates.map(c ⇒ (c.dimension.name, c.id))
  }
  def empty = new PointDefinition {
    override def apply(ds: Set[Dimension]) = Point.empty
    override def raw = Map.empty
  }
}

object Bindables {
  implicit object PointQueryStringBindable extends QueryStringBindable[PointDefinition] with CoordinateFactory {
    override def bind(key: String, params: Map[String, Seq[String]]) = {
      def long(s: String) = catching(classOf[NumberFormatException]).opt(s.toLong)
      try {
        val prefix = key + "."
        val rawValues = params.filter(_._1.startsWith(prefix)).map(v ⇒ (v._1.drop(prefix.length), v._2)).filterNot(_._1.isEmpty).
          flatMap(v ⇒ v._2.map((v._1, _))).map(v ⇒ (dec(v._1), dec(v._2))).
          flatMap(v ⇒ long(v._2).map((v._1, _)))
        val definition = new PointDefinition {
          override def apply(ds: Set[Dimension]) = {
            rawValues.
              flatMap(v ⇒ ds.find(_.name == v._1).flatMap(_.get(v._2))).
              foldLeft(Point.empty)(_ + _)
          }
          override def raw = rawValues
        }
        Some(Right(definition))
      } catch {
        case r: RuntimeException ⇒ Some(Left(r.toString))
      }
    }

    override def unbind(key: String, value: PointDefinition) = {
      value.raw.map(e ⇒ (enc(e._1), enc(e._2.toString))).
        map(e ⇒ s"$key.${e._1}=${e._2}").mkString("&")
    }

    override def javascriptUnbind: String = "window.fudim.point.unbind"

    private def enc(s: String) = URLEncoder.encode(s, "UTF-8")
    private def dec(s: String) = URLDecoder.decode(s, "UTF-8")
  }

}
