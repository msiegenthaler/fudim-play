package models

import cube._
import domain._

/** A fact has values for each coordinate in dimensions. */
trait FudimFact[T] extends RenderFact[T] {
  protected def updateCube(aggregation: Aggregation): Unit

  def aggregation: Aggregation = data match { case Aggregation(aggr) ⇒ aggr }
  def aggregation_=(aggr: Aggregation) = updateCube(aggr)

  def addDimension(moveTo: Coordinate): Unit
  def removeDimension(keepAt: Coordinate): Unit
}