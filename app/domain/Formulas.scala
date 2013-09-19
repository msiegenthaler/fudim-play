package domain

import cube._

/** Combines the values of multiple cubes at the same point with a function. */
object PointFoldFormula {
  def apply[A, B](f: Traversable[Option[A]] => Option[B], toType: DataType[B])(ofNames: Traversable[String], ofType: DataType[A], over: Traversable[Dimension]): Formula[B] = {
    val refs = ofNames.map(CubeRef(_, ofType)).toSet
    PointFoldFormulaImpl(f, toType, refs, ofType, over.toSet)
  }

  private case class PointFoldFormulaImpl[A, B](f: Traversable[Option[A]] => Option[B], toType: DataType[B],
                                                refs: Set[CubeRef[A]], ofType: DataType[A], dimensions: Set[Dimension]) extends Formula[B] {
    override val references = refs.asInstanceOf[Set[CubeRef[_]]]
    override def bind(cubes: Cubes) = {
      val cs = refs.map(ref => (ref, cubes.get(ref)))
      val missing = cs.find(_._2.isEmpty)
      if (!missing.isEmpty) throw new IllegalArgumentException(s"Unresolved dependencies to: ${missing.mkString(", ")}")
      val deps = cs.map(_._2.get)
      point => {
        if (point.definesExactly(dimensions)) {
          val values = deps.toList.map(_.get(point))
          f(values)
        } else None
      }
    }
  }
}
