package models

import base._
import cube._
import domain._

trait Fact[T] {
  def name: String
  /** Version of the fact itself (does not include the data, for that use data.version). */
  def version: Version

  def data: VersionedCube[T]
  def dataType: FudimDataType[T]
  def rendered = data.map(dataType.render)

  def editor: Option[CubeEditor[T]]

  def aggregation: Aggregation[T]
  def aggregation_=(aggr: Aggregation[T]): Unit @tx

  def dimensions: Set[Dimension] = data.dimensions
  def addDimension(moveTo: Coordinate): Unit @tx
  def removeDimension(keepAt: Coordinate): Unit @tx
}

trait FactRepo {
  def get(name: String): Option[Fact[_]]
  def all: Iterable[Fact[_]]

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]): Fact[T] @tx
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]): Fact[T] @tx

  def remove(name: String): Unit @tx
}
