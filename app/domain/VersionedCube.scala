package domain

import cube._

/** Cube that tracks a version for each value. */
trait VersionedCube[T, Version] extends Cube[T] {
  type Self <: VersionedCube[T, Version]

  /** Version of the newest element in this cube (view). Slice, dice and after are respected. */
  def version: Version

  /** Version the value at that point. Even points with no value do have a version. */
  def version(p: Point): Version = slice(p).version
  /**

   * View of the cube that only contains values newer than the specified version.
   * Especially usefull with in combination with sparse (after(x).sparse returns all values that were changed since x).
   */
  def after(v: Version): Self
}
