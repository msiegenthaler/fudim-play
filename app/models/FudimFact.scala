package models

import cube._
import domain._

trait FudimFact[T] extends RenderFact[T] {
  override def dataType: FudimDataType[T]

  override lazy val rendered = data.map(dataType.render)

  def editor: Option[CubeEditor[T]]

  protected def updateCube(aggregation: Aggregation[T]): Unit

  def aggregation: Aggregation[T] = data match {
    case Aggregation(aggr) ⇒ aggr
  }
  def aggregation_=(aggr: Aggregation[T]) = updateCube(aggr)

  def addDimension(moveTo: Coordinate): Unit
  def removeDimension(keepAt: Coordinate): Unit
}

trait FudimFactRepo {
  def get(name: String): Option[FudimFact[_]]
  def all: Iterable[FudimFact[_]]

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregator: Option[Aggregator[T]]): FudimFact[T]
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregator: Option[Aggregator[T]]): FudimFact[T]

  def remove(name: String)
}
