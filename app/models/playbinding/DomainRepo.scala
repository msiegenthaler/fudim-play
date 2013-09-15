package models
package playbinding

import cube._
import db._
import dbcube.DatabaseCubeRepo

object DomainRepo extends DatabaseDomainRepo with PlayDatabaseRepo {
  override protected def dimensionRepo(d: DomainId) = {
    new DatabaseDimensionRepo with PlayDatabaseRepo {
      override def domain = d
    }
  }
  override protected def factRepo(d: DomainId) = {
    val dims = dimensionRepo(d)
    new DatabaseFactRepo with PlayDatabaseRepo {
      override def domain = d
      override val databaseCubeRepo = {
        new DatabaseCubeRepo with PlayDatabaseRepo {
          override def dimension(name: String) = dims.get(name)
        }
      }
      override val jsonCubeMapperRepo = new JsonCubeMapperRepository {
        override val mappers = databaseCubeRepo.json :: CubeDecorator.json(JsonMappers.decorator, this) :: Nil
      }
    }
  }
}