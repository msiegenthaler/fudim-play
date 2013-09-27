package models.dbcube

import anorm.{RowParser, ParameterValue}
import cube._
import domain.{CubeDataStore, DataType}


trait DatabaseCubeDataStore[T] extends CubeDataStore[T] {
  def id: Long

  protected[dbcube] def storeType: StoreDataType[T]
  override def dataType = storeType.dataType
  protected[dbcube] def table: String
  protected[dbcube] def dims: Map[Dimension, String]

  protected[dbcube] def create(): Unit
  protected[dbcube] def drop(): Unit


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
