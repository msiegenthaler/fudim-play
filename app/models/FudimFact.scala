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

trait FudimFactRepo {
  def get[T](domain: DomainId, name: String): Option[FudimFact[_]]
  def all(domain: DomainId): Iterable[FudimFact[_]]
  def all: Iterable[FudimFact[_]]

  def createDatabaseBacked[T](domain: DomainId, name: String, dataType: DataType[T],
    dimensions: Set[Dimension], aggregator: Option[Aggregator[T]]): FudimFact[T]
}