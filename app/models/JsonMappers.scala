package models

import models.cube._
import models.cube.db.DatabaseCube
import models.json._

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

  def cube = new JsonCubeMapperRepository {
    override val mappers = DatabaseCube.json :: CubeDecorator.json(decorator, this) :: Nil
  }
}