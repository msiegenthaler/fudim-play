package domain

import cube._

/** Cube that tracks a version for each value. */
trait VersionedCube[T] extends Cube[T] {
  type Self <: VersionedCube[T]

  /** Version of the newest element in this cube (view). Slice, dice and after are respected. */
  def version: Version

  /** Version the value at that point. Even points with no value do have a version. */
  def version(p: Point): Version = slice(p).version

  override def map[A](f: T ⇒ A): VersionedCube[A] = VersionedCube.mapped(this, f)
}

object VersionedCube {
  def mapped[A, B](cube: VersionedCube[A], f: A ⇒ B): VersionedCube[B] = MappedCube(cube, f)

  private case class MappedCube[A, B](cube: VersionedCube[A], f: A ⇒ B) extends VersionedCube[B] {
    override type Self = VersionedCube[B]
    override def version = cube.version
    override def get(at: Point) = cube.get(at).map(f)
    override def dense = cube.dense.map(t ⇒ (t._1, t._2.map(f)))
    override def sparse = cube.sparse.map(t ⇒ (t._1, f(t._2)))
    override def values = cube.values.map(f)
    override def slice = cube.slice
    override def raw = wrap(cube.raw)
    override def dimensions = cube.dimensions
    override def slice(to: Point) = wrap(cube.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(cube.dice(dimension, filter))
    protected def wrap(cube: VersionedCube[A]): Self = copy(cube = cube)
  }
}

object VersionedCubeDecorator {
  def apply[T](cube: VersionedCube[T], decorator: CubeDecorator[T]): CubeDecoratorCube[T] with VersionedCube[T] = {
    new VersionedCubeWithDecorator(cube, decorator)
  }
  def unapply[T](cube: VersionedCube[T]): Option[(VersionedCube[T], CubeDecorator[T])] = cube match {
    case c: VersionedCubeWithDecorator[T] ⇒ Some(c.underlying, c.decorator)
    case _ ⇒ None
  }

  private class VersionedCubeWithDecorator[T](val underlying: VersionedCube[T], val decorator: CubeDecorator[T])
    extends CubeDecoratorCube[T] with AbstractDecoratedCube[T] with VersionedCube[T] {
    override type Self = VersionedCubeWithDecorator[T]
    override type Underlying = Cube[T]
    override protected def wrap(c: underlying.Self) = new VersionedCubeWithDecorator(c, decorator)
    override def version = underlying.version
    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def toString = s"VersionedCubeWithDecorator($underlying, $decorator)"
  }
}