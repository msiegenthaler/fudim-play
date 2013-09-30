package models

import base._
import cube._
import domain._

trait FudimFact[T] extends RenderFact[T] {
  override def dataType: FudimDataType[T]

  override lazy val rendered = data.map(dataType.render)

  def editor: Option[CubeEditor[T]]

  def aggregation: Aggregation[T]
  def aggregation_=(aggr: Aggregation[T]): Tx

  def addDimension(moveTo: Coordinate): Tx
  def removeDimension(keepAt: Coordinate): Tx
}

trait FudimFactRepo {
  def get(name: String): Option[FudimFact[_]]
  def all: Iterable[FudimFact[_]]

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]): <>[FudimFact[T]]
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]): <>[FudimFact[T]]

  def remove(name: String): Tx
}
