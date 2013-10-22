package models

import base._
import cube._
import domain.Version

trait FudimDimension extends Dimension {
  /** Version of the dimension (incl. its data). */
  def version: Version

  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate @tx
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate @tx
}

trait FudimDimensionRepo extends DimensionRepository {
  override def get(name: String): Option[FudimDimension] = all.find(_.name == name)
  override def all: Iterable[FudimDimension]

  def create(name: String): FudimDimension @tx
  def remove(name: String): Unit @tx
}
