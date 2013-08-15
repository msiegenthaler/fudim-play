package cube

trait Formula[T] {
  def references: Set[CubeRef]
  def dimensions: Set[Dimension]
  def apply(deps: Cubes): BoundFormula[T]
}

/** Cube based on a calculation */
object FormulaCube {
  def apply[D](formula: Formula[D], cubes: Cubes): Cube[D] = {
    val submodel = cubes.filterKeys(formula.references.contains)
    if (formula.references.forall(submodel.contains)) {
      FormulaCube(formula(submodel), formula.dimensions)
    } else {
      val missing = formula.references -- submodel.keys
      throw new IllegalArgumentException(s"Formula $formula has unsatisfied dependencies: $missing")
    }
  }

  private case class FormulaCube[D](formula: BoundFormula[D], allDimensions: Set[Dimension], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[D] {
    protected override type Self = FormulaCube[D]
    protected override def derive(slice: Point = slice, filters: DimensionFilter = filters) = {
      copy(slice = slice, filters = filters)
    }
    override def get(at: Point) = formula(at)
    override def dense = allPoints.map(p ⇒ (p, formula(p)))
  }
}