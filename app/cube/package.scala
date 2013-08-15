import support.{ JsonMapper, JsonMapperRepository }

package object cube {
  type DimensionFilter = Map[Dimension, Coordinate ⇒ Boolean]

  type JsonCubeMapper = JsonMapper[Cube[_]]
  type JsonCubeMapperRepository = JsonMapperRepository[Cube[_]]
  type JsonCubeDecoratorMapper = JsonMapper[CubeDecorator[_]]
  type JsonCubeDecoratorMapperRepository = JsonMapperRepository[CubeDecorator[_]]
  type JsonAggregatorMapper = JsonMapper[Aggregator[_]]
  type JsonAggregatorMapperRepository = JsonMapperRepository[Aggregator[_]]
}