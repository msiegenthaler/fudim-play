package domain

import cube.Cube

case class CubeRef[T](name: String, dataType: DataType[T])

trait Cubes {
  def refs: Set[CubeRef[_]]
  def get[T](ref: CubeRef[T]): Option[Cube[T]]

  /** Narrows down this cube to only contain the cubes in refs. */
  def narrow(to: Traversable[CubeRef[_]]): Option[Cubes] = {
    if (to.forall(refs.contains)) Some(NarrowedCubes(this, refs.toSet))
    else None
  }
}

private case class NarrowedCubes(base: Cubes, refs: Set[CubeRef[_]]) extends Cubes {
  override def get[T](ref: CubeRef[T]) = {
    if (refs.contains(ref)) base.get(ref)
    else None
  }
}
