package models.dbcube

import anorm.{RowParser, ParameterValue}
import cube._
import domain.{CubeDataStore, DataType}


trait DatabaseCubeDataStore[T] extends CubeDataStore[T] {
  def storeType: StoreDataType[T]
  override def dataType = storeType.dataType

  /**
   * Creates a copy of this cube with identical data but an additional dimension.
   * Existing data will be assigned the specified coordinate in the new dimension.
   */
  def copyAndAddDimension(moveTo: Coordinate): DatabaseCubeDataStore[T]
  /**
   * Creates a copy of this cube with identical data but a removed dimension.
   * Only the data at specified coordinate will be kept.
   */
  def copyAndRemoveDimension(keepAt: Coordinate): DatabaseCubeDataStore[T]
}

trait StoreDataType[T] {
  def name = dataType.name
  val dataType: DataType[T]

  val sqlType: String
  def fromDb(name: String): RowParser[T]
  def toDb(value: T): ParameterValue[_]

  override def toString = dataType.toString
}
