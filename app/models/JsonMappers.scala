package models

import models.cube.JsonCubeMapperRepository
import models.cube.db.DatabaseCube

object JsonMappers {
  object CubeMapper extends JsonCubeMapperRepository {
    override val mappers = DatabaseCube.json :: Nil
  }
}