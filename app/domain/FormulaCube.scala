package domain

import play.api.libs.json.Json
import cube._
import support.{ JsonMapper, JsonMapperRepository }

/** Calculates points based on data in other cubes. */
trait Formula[T] {
  /** The resulting function must be pure and must only depend on data in the referenced cubes. */
  def bind(cubes: Cubes): Point ⇒ Option[T]
  def dimensions: Set[Dimension]
  def references: Iterable[CubeRef[_]]
}

/** Cube based on a calculation. */
object FormulaCube {
  def apply[T](formula: Formula[T], cubes: Cubes): Cube[T] = FormulaCube(formula, formula.bind(cubes))

  def unapply[T](cube: Cube[T]): Option[Formula[T]] = cube match {
    case FormulaCube(formula, _, _, _) ⇒ Some(formula)
    case _ ⇒ None
  }

  def json(formulaRepo: JsonFormulaMapperRepository)(cubes: Cubes) = new JsonCubeMapper {
    import scalaz._
    import Scalaz._
    override val id = "formulaCube"
    override def parser = json ⇒
      for {
        formula ← formulaRepo.parse(json \ "formula")
      } yield apply(formula, cubes)
    override def serializer = {
      case FormulaCube(formula, _, _, _) ⇒
        for {
          f ← formulaRepo.serialize(formula)
        } yield Json.obj("formula" -> f)
    }
  }

  private case class FormulaCube[T](formula: Formula[T], bound: Point ⇒ Option[T], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[T] {
    override protected type Self = FormulaCube[T]
    override protected def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
    override def get(at: Point) = bound(at)
    override def dense = allPoints.map(p ⇒ (p, bound(p)))
    override val allDimensions = formula.dimensions
  }
}
