import support.{ JsonMapper, JsonMapperRepository }

package object cube {
  type JsonCubeMapper = JsonMapper[Cube[_]]
  type JsonCubeMapperRepository = JsonMapperRepository[Cube[_]]
}