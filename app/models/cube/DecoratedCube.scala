package models.cube

import models._

/** Cube that decorates another cube. I.e. to add aggragation of values. */
trait DecoratedCube[D] extends Cube[D] {
  protected override type Self <: DecoratedCube[D]
  type Underlying <: Cube[D]
  val underlying: Underlying
}

/** Decorator for a cube, for use with DecoratedCube.apply(). */
trait CubeDecorator[D] {
  def get(decoratee: Cube[D])(at: Point) = decoratee.get(at)
  def dense(decoratee: Cube[D]) = decoratee.dense
  def sparse(decoratee: Cube[D]) = decoratee.sparse
}
/** Decorator for a cube that also handles changes to the cube. Use with DecoratedCube.apply(). */
trait EditableCubeDecorator[D] extends CubeDecorator[D] {
  def isSettable(decoratee: EditableCube[D])(at: Point) = decoratee.isSettable(at)
  def set(decoratee: EditableCube[D])(at: Point, value: Option[D]) = decoratee.set(at, value)
  def setAll(decoratee: EditableCube[D])(value: Option[D]) = decoratee.setAll(value)
}
object EditableCubeDecorator {
  def from[D](d: CubeDecorator[D]): EditableCubeDecorator[D] = d match {
    case d: EditableCubeDecorator[D] ⇒ d
    case d ⇒ new EditableCubeDecorator[D] {
      override def get(decoratee: Cube[D])(at: Point) = d.get(decoratee)(at)
      override def dense(decoratee: Cube[D]) = d.dense(decoratee)
      override def sparse(decoratee: Cube[D]) = d.sparse(decoratee)
    }
  }
}
/** Decorator that does not change any behaviour. */
case class NoDecorator[D]() extends EditableCubeDecorator[D]

object DecoratedCube {
  def apply[D](cube: Cube[D], decorator: CubeDecorator[D]): DecoratedCube[D] = cube match {
    case cube: EditableCube[D] ⇒ new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
    case cube ⇒ new CubeWithDecorator(cube, decorator)
  }
  def apply[D](cube: EditableCube[D], decorator: CubeDecorator[D]): DecoratedCube[D] with EditableCube[D] = {
    new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
  }

  /** Removes all decoration from a cube. */
  def undecorate[D](cube: Cube[D]): Cube[D] = cube match {
    case c: DecoratedCube[D] ⇒ undecorate(c.underlying)
    case c ⇒ c
  }

  private class CubeWithDecorator[D](val underlying: Cube[D], decorator: CubeDecorator[D]) extends DecoratedCube[D] {
    override protected type Self = CubeWithDecorator[D]
    override type Underlying = Cube[D]
    private def wrap(c: Cube[D]) = new CubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
  }
  private class EditableCubeWithDecorator[D](override val underlying: EditableCube[D], decorator: EditableCubeDecorator[D]) extends EditableCube[D] with DecoratedCube[D] {
    override protected type Self = EditableCubeWithDecorator[D]
    override type Underlying = EditableCube[D]
    private def wrap(c: EditableCube[D]) = new EditableCubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
    override def isSettable(at: Point) = decorator.isSettable(underlying)(at)
    override def set(at: Point, value: Option[D]) = decorator.set(underlying)(at, value)
    override def setAll(value: Option[D]) = decorator.setAll(underlying)(value)
  }
}