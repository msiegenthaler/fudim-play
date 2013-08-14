import support.{ JsonMapper, JsonMapperRepository }

package object cube {
  type DimensionFilter = Map[Dimension, Coordinate ⇒ Boolean]

  type CubeModel = Map[String, Cube[_]]
  type Formula[D] = CubeModel ⇒ BoundFormula[D]
  type BoundFormula[D] = Point ⇒ Option[D]

  type JsonCubeMapper = JsonMapper[Cube[_]]
  type JsonCubeMapperRepository = JsonMapperRepository[Cube[_]]
  type JsonCubeDecoratorMapper = JsonMapper[CubeDecorator[_]]
  type JsonCubeDecoratorMapperRepository = JsonMapperRepository[CubeDecorator[_]]
  type JsonAggregatorMapper = JsonMapper[Aggregator[_]]
  type JsonAggregatorMapperRepository = JsonMapperRepository[Aggregator[_]]
}