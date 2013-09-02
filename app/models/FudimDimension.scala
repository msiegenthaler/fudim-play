package models

import cube.{ Dimension, Coordinate }

trait FudimDimension extends Dimension {
  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate
}