package models

import base._
import cube._
import domain._

trait FudimFact[T] extends RenderFact[T] {
  override def dataType: FudimDataType[T]

  override lazy val rendered = data.map(dataType.render)

  def editor: Option[CubeEditor[T]]

  def aggregation: Aggregation[T]
  def aggregation_=(aggr: Aggregation[T]): Unit@tx

  def addDimension(moveTo: Coordinate): Unit@tx
  def removeDimension(keepAt: Coordinate): Unit@tx
}

trait FudimFactRepo {
  def get(name: String): Option[FudimFact[_]]
  def all: Iterable[FudimFact[_]]

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]): FudimFact[T]@tx
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]): FudimFact[T]@tx

  def remove(name: String): Unit@tx
}
