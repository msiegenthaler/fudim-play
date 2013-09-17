package models

import scala.util.control.Exception._
import scalaz._
import Scalaz._
import _root_.play.api.libs.json._
import cube._
import support.ObjectJsonMapper
import support.JsonMapper

class Aggregation[T] private (val name: String, val aggregator: Option[Aggregator[T]]) {
  override def toString = name
}

object Aggregation {
  private class NoAggregation[T] extends Aggregation[T]("No Aggregation", None)
  def none[T]: Aggregation[T] = new NoAggregation

  val sum = Aggregation("sum", Aggregators.sumLong)
  val product = Aggregation("product", Aggregators.fold(1L)(_ * _))
  val average = Aggregation("average", Aggregators.avgLong)

  val concat = Aggregation("concat", Aggregators.fold("")(_ + _))

  val all = List[Aggregation[_]](none, sum, product, average, concat)

  def unapply[T](cube: Cube[T]): Option[Aggregation[T]] = cube match {
    case CubeDecorator(_, Aggregator(aggr)) ⇒
      all.
        find(_.aggregator.filter(_ == aggr).isDefined).
        map(_.asInstanceOf[Aggregation[T]])
    case _ ⇒ Some(none)
  }

  private def apply[T](name: String, aggr: Aggregator[T]) = {
    JsonMappers.registerAggregator(ObjectJsonMapper(name, aggr))
    new Aggregation(name, Some(aggr))
  }
}
