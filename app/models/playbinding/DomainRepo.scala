package models
package playbinding

import cube._
import db._
import dbcube.DatabaseCubeRepo

object DomainRepo extends FudimDomainRepo {
  private object DimensionRepo extends DatabaseDimensionRepo with PlayDatabaseRepo
  private object DbCubeRepo extends DatabaseCubeRepo with PlayDatabaseRepo
  private object FactRepo extends DatabaseFactRepo with PlayDatabaseRepo {
    override def databaseCubeRepo = DbCubeRepo
    override def jsonCubeMapperRepo = new JsonCubeMapperRepository {
      override val mappers = DbCubeRepo.json :: CubeDecorator.json(JsonMappers.decorator, this) :: Nil
    }
  }
}