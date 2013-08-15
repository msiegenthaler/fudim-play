package cube

import support.{ JsonMapper, JsonMapperRepository }
import play.api.libs.json.{ Json, JsValue }

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
      FormulaCube(formula, formula(submodel))
    } else {
      val missing = formula.references -- submodel.keys
      throw new IllegalArgumentException(s"Formula $formula has unsatisfied dependencies: $missing")
    }
  }

  def json(formulaRepo: JsonMapperRepository[Formula[_]])(cubes: Cubes) = new JsonCubeMapper {
    import scalaz._
    import Scalaz._
    override val id = "formulaCube"
    override def parser = json ⇒ for {
      formula ← formulaRepo.parse(json \ "formula")
    } yield apply(formula, cubes)
    override def serializer = {
      case FormulaCube(formula, _, _, _) ⇒
        for {
          f ← formulaRepo.serialize(formula)
        } yield Json.obj("formula" -> f)
    }
  }

  private case class FormulaCube[D](formula: Formula[D], bound: BoundFormula[D], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[D] {
    protected override type Self = FormulaCube[D]
    protected override def derive(slice: Point = slice, filters: DimensionFilter = filters) = {
      copy(slice = slice, filters = filters)
    }
    override def get(at: Point) = bound(at)
    override def dense = allPoints.map(p ⇒ (p, bound(p)))
    override val allDimensions = formula.dimensions
  }
}