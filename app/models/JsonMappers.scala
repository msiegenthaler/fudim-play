package models

import cube._
import models.dbcube.DatabaseCube
import support.JsonMapperRepository
import support.JsonMapper

object JsonMappers {
  @volatile
  private var aggregatorMappers: List[JsonMapper[Aggregator[_]]] = Nil
  def aggregator = new JsonMapperRepository[Aggregator[_]] {
    override val mappers = aggregatorMappers
  }
  def registerAggregator(mapper: JsonMapper[Aggregator[_]]) {
    aggregatorMappers = aggregatorMappers ::: mapper :: Nil
  }

  def decorator = new JsonMapperRepository[CubeDecorator[_]] {
    override val mappers = Aggregator.json(aggregator) :: Nil
  }
}