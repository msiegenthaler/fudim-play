package domain

import cube._

/** Calculates points based on data in other cubes. */
trait Formula[T] {
  /** The resulting function must be pure and must only depend on data in the referenced cubes. */
  def bind(cubes: Cubes): Point => Option[T]
  def dimensions: Set[Dimension]
  def references: Set[CubeRef[_]]
}

/** Cube based on a calculation. */
object FormulaCube {
  def apply[T](formula: Formula[T], cubes: Cubes): Cube[T] = FormulaCube(formula, formula.bind(cubes))

  private case class FormulaCube[T](formula: Formula[T], bound: Point => Option[T], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[T] {
    override protected type Self = FormulaCube[T]
    override protected def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
    override def get(at: Point) = bound(at)
    override def dense = allPoints.map(p ⇒ (p, bound(p)))
    override val allDimensions = formula.dimensions
  }
}

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
