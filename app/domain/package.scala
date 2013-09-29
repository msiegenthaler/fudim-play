import support.{JsonMapperRepository, JsonMapper}

package object domain {
  type JsonFormulaMapper = JsonMapper[Formula[_]]
  type JsonFormulaMapperRepository = JsonMapperRepository[Formula[_]]
  type JsonCubeDSMapper = JsonMapper[CubeDataStore[_]]
  type JsonCubeDSMapperRepository = JsonMapperRepository[CubeDataStore[_]]
}
