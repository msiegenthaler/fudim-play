import support.{JsonMapperRepository, JsonMapper}

package object domain {
  type JsonFormulaMapper = JsonMapper[Formula[_]]
  type JsonFormulaMapperRepository = JsonMapperRepository[Formula[_]]
}
