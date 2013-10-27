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