package domain

import base._
import cube._

/** Stores the data for a cube (storage backend). */
trait CubeDataStore[T] {
  def id: Long

  def dataType: DataType[T]

  def cube: VersionedCube[T]
  def editor: CubeEditor[T]
}

trait CubeDataStoreRepo {
  type CDS[T] <: CubeDataStore[T]

  def get(id: Long): Option[CDS[_]]

  def get[T](id: Long, dataType: DataType[T]): Option[CDS[T]] = get(id).map { cds ⇒
    if (cds.dataType != dataType)
      throw new IllegalArgumentException(s"type of cube $id (type ${cds.dataType} does not match expected type $dataType")
    cds.asInstanceOf[CDS[T]]
  }

  def create[T](dimensions: Set[Dimension], dataType: DataType[T]): CDS[T] @tx

  def remove(id: Long): Unit @tx
}

trait CopyableCubeDataStore[T] extends CubeDataStore[T] {
  type Self <: CopyableCubeDataStore[T]

  /**
   * Creates a copy of this cube with identical data.
   * Dimensions in the 'add' point will be added, the existing data will be added at the specified Coordinates.
   * Dimension in the 'remove' point will be removed, (only) the data at the specified coordinate will be preserved.
   * Add and remove must not overlap, all dimensions added must not yet be, all dimensions removes must be in the data.
   */
  def copy(add: Point = Point.empty, remove: Point = Point.empty): Self @tx
}

trait CopyableCubeDataStoreRepo extends CubeDataStoreRepo {
  type CDS[T] <: CopyableCubeDataStore[T]
}
