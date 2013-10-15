import support.{JsonMapperRepository, JsonMapper}

package object domain {
  type JsonFormulaMapper = JsonMapper[Formula[_]]
  type JsonFormulaMapperRepository = JsonMapperRepository[Formula[_]]
  type JsonCubeDSMapper[Version] = JsonMapper[CubeDataStore[_, Version]]
  type JsonCubeDSMapperRepository[Version] = JsonMapperRepository[CubeDataStore[_, Version]]
}
