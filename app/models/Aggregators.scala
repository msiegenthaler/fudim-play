package models

import scala.util.control.Exception._
import scalaz._
import Scalaz._
import play.api.libs.json._
import models.cube.{ Aggregators ⇒ A, Aggregator }
import models.json.JsonMapper

object Aggregators {
  val all = sum :: Nil

  val sum = {
    def sumIfNumber(oa: Option[String], b: String): Option[String] = {
      for {
        a ← oa
        na ← catching(classOf[NumberFormatException]).opt(a.toLong)
        nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
      } yield (na + nb).toString
    }
    register("sum", A.fold(Some("0"))(sumIfNumber))
  }

  private def register[T](name: String, aggr: Aggregator[T]) = {
    JsonMappers.registerAggregator(new JsonMapper[Aggregator[_]] {
      override val id = name
      override def parser = json ⇒ aggr.success
      override def serializer = { case `aggr` ⇒ JsArray().success }
    })
    aggr
  }

}