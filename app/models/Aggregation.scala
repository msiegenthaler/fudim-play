package models

import scala.util.control.Exception._
import scalaz._
import Scalaz._
import play.api.libs.json._
import models.cube.{ Aggregators ⇒ A, Aggregator }
import models.json.JsonMapper

trait Aggregation {
  val name: String
  val aggregator: Aggregator[String]
}

object Aggregation {
  val sum = {
    def sumIfNumber(oa: Option[String], b: String): Option[String] = {
      for {
        a ← oa
        na ← catching(classOf[NumberFormatException]).opt(a.toLong)
        nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
      } yield (na + nb).toString
    }
    Aggregation("sum", A.fold(Some("0"))(sumIfNumber))
  }

  val all = sum :: Nil

  private def apply(name: String, aggr: Aggregator[String]) = {
    JsonMappers.registerAggregator(new JsonMapper[Aggregator[_]] {
      override val id = name
      override def parser = json ⇒ aggr.success
      override def serializer = { case `aggr` ⇒ JsArray().success }
    })
    val n = name
    new Aggregation {
      override val name = n
      override val aggregator = aggr
      override def toString = name
    }
  }
}