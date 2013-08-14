package cube

/** Cube based on a calculation */
object FormulaCube {
  def apply[D](formula: BoundFormula[D], dimensions: Set[Dimension]): Cube[D] = {
    FormulaCube(formula, dimensions)
  }

  private case class FormulaCube[D](formula: BoundFormula[D], allDimensions: Set[Dimension], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[D] {
    protected override type Self = FormulaCube[D]
    protected override def derive(slice: Point = slice, filters: DimensionFilter = filters) = {
      copy(slice = slice, filters = filters)
    }
    override def get(at: Point) = formula(at)
    override def dense = allPoints.map(p => (p, formula(p)))
  }
}