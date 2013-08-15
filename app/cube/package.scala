import support.{ JsonMapper, JsonMapperRepository }

package object cube {
  type DimensionFilter = Map[Dimension, Coordinate ⇒ Boolean]

  /** Reference to a cube. */
  case class CubeRef(name: String, tpe: Class[_]) {
    override def toString = name
  }

  type Cubes = Map[CubeRef, Cube[_]]

  type BoundFormula[D] = Point ⇒ Option[D]

  type JsonCubeMapper = JsonMapper[Cube[_]]
  type JsonCubeMapperRepository = JsonMapperRepository[Cube[_]]
  type JsonCubeDecoratorMapper = JsonMapper[CubeDecorator[_]]
  type JsonCubeDecoratorMapperRepository = JsonMapperRepository[CubeDecorator[_]]
  type JsonAggregatorMapper = JsonMapper[Aggregator[_]]
  type JsonAggregatorMapperRepository = JsonMapperRepository[Aggregator[_]]
}