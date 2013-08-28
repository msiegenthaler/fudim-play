package models

import cube._
import domain._

trait FudimFact[T] extends RenderFact[T] {
  protected def updateCube(aggregation: Aggregation): Unit

  def aggregation: Aggregation = data match { case Aggregation(aggr) ⇒ aggr }
  def aggregation_=(aggr: Aggregation) = updateCube(aggr)

  def addDimension(moveTo: Coordinate): Unit
  def removeDimension(keepAt: Coordinate): Unit
}

trait FudimDimension extends Dimension {
  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate
}