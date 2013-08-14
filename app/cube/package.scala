import support.{ JsonMapper, JsonMapperRepository }

package object cube {
  type DimensionFilter = Map[Dimension, Coordinate ⇒ Boolean]

  type JsonCubeMapper = JsonMapper[Cube[_]]
  type JsonCubeMapperRepository = JsonMapperRepository[Cube[_]]
}