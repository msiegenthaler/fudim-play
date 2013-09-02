package models

import cube._
import domain._

trait FudimFact[T] extends RenderFact[T] {
  def dataType: DataType[T]

  override lazy val rendered = data.map(dataType.render)

  protected def updateCube(aggregation: Aggregation[T]): Unit

  def aggregation: Aggregation[T] = data match { case Aggregation(aggr) ⇒ aggr }
  def aggregation_=(aggr: Aggregation[T]) = updateCube(aggr)

  def addDimension(moveTo: Coordinate): Unit
  def removeDimension(keepAt: Coordinate): Unit
}