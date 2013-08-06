package models

import scala.util.control.Exception._
import scalaz._
import Scalaz._
import play.api.libs.json._
import models.cube._
import models.json.JsonMapper

class Aggregation private (val name: String, val aggregator: Option[Aggregator[String]]) {
  def onCube(cube: Cube[String]): Cube[String] = {
    val raw = CubeDecorator.undecorate(cube)
    aggregator.map(a ⇒ CubeDecorator(raw, a)).getOrElse(raw)
  }
  override def toString = name
}

object Aggregation {
  val none = new Aggregation("No Aggregation", None)

  val sum = {
    def sumIfNumber(oa: Option[String], b: String): Option[String] = {
      for {
        a ← oa
        na ← catching(classOf[NumberFormatException]).opt(a.toLong)
        nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
      } yield (na + nb).toString
    }
    Aggregation("sum", Aggregators.fold(Some("0"))(sumIfNumber))
  }

  val product = {
    def productIfNumber(oa: Option[String], b: String): Option[String] = {
      for {
        a ← oa
        na ← catching(classOf[NumberFormatException]).opt(a.toLong)
        nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
      } yield (na * nb).toString
    }
    Aggregation("product", Aggregators.fold(Some("1"))(productIfNumber))
  }

  def concat = {
    Aggregation("concat", Aggregators.fold("")(_ + _))
  }

  val all = none :: sum :: product :: concat :: Nil

  def unapply(cube: Cube[String]): Option[Aggregation] = cube match {
    case CubeDecorator(Aggregator(aggr)) ⇒ all.find(_.aggregator.filter(_ == aggr).isDefined)
    case _ ⇒ None
  }

  private def apply(name: String, aggr: Aggregator[String]) = {
    JsonMappers.registerAggregator(new JsonMapper[Aggregator[_]] {
      override val id = name
      override def parser = json ⇒ aggr.success
      override def serializer = { case `aggr` ⇒ JsArray().success }
    })
    new Aggregation(name, Some(aggr))
  }
}