package domain

import cube._

/** Stores the data for a cube (storage backend). */
trait CubeDataStore[T] {
  def id: Long

  def dataType: DataType[T]

  def cube: Cube[T]
  def editor: CubeEditor[T]
}

trait CubeDataStoreRepo {
  def get(id: Long): Option[CubeDataStore[_]]

  def get[T](id: Long, dataType: DataType[T]): Option[CubeDataStore[T]] = get(id).map { cds =>
    if (cds.dataType != dataType)
      throw new IllegalArgumentException(s"type of cube $id (type ${cds.dataType} does not match expected type $dataType")
    cds.asInstanceOf[CubeDataStore[T]]
  }

  def create[T](dimensions: Set[Dimension], dataType: DataType[T]): CubeDataStore[T]

  def remove(id: Long): Unit
}
