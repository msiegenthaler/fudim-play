package domain

import cube.Dimension

/** Commonly used formulas. */
object Formulas {
  /** Combines the values of multiple cubes at the same point with a function. */
  def pointFold[A, B](f: Traversable[Option[A]] => Option[B], toType: DataType[B])(ofNames: Traversable[String], ofType: DataType[A], over: Traversable[Dimension]): Formula[B] = new Formula[B] {
    private def dataType = toType
    private val refs = ofNames.map(CubeRef(_, ofType)).toSet
    override val references = refs.asInstanceOf[Set[CubeRef[_]]]
    override val dimensions = over.toSet
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
