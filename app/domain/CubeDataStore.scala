package domain

import cube._

/** Stores the data for a cube (storage backend). */
trait CubeDataStore[T] {
  def dataType: DataType[T]

  def cube: Cube[T]
  def editor: CubeEditor[T]
}
