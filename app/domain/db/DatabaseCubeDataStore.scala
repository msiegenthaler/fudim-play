package domain
package db

import anorm.{ RowParser, ParameterValue }

trait DatabaseCubeDataStore[T] extends CopyableCubeDataStore[T] {
  override type Self <: DatabaseCubeDataStore[T]
  def storeType: StoreDataType[T]
  override def dataType = storeType.dataType
}

trait StoreDataType[T] {
  def name = dataType.name
  val dataType: DataType[T]

  val sqlType: String
  def fromDb(name: String): RowParser[T]
  def toDb(value: T): ParameterValue[_]

  override def toString = dataType.toString
}
