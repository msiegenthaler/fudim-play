package domain

import cube._

trait Fact[T] {
  def name: String
  def dataType: DataType[T]

  def dimensions = data.dimensions
  def data: Cube[T]
}

/** Fact that also has a string representation. */
trait RenderFact[T] extends Fact[T] {
  def rendered: Cube[String]
}
