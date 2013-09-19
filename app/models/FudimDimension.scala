package models

import cube._

trait FudimDimension extends Dimension {
  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate
}

trait FudimDimensionRepo extends DimensionRepository {
  override def get(name: String): Option[FudimDimension] = all.find(_.name == name)
  override def all: Iterable[FudimDimension]

  def create(name: String): FudimDimension
  def remove(name: String): Unit
}
