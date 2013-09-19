package models
package playbinding

import cube._
import domain._
import db._
import dbcube.DatabaseCubeRepo
import models.DomainId

object DomainRepo extends DatabaseDomainRepo with PlayDatabaseRepo {
  protected val dataTypes = FudimDataTypes

  override protected def dimensionRepo(d: DomainId) = {
    new DatabaseDimensionRepo with PlayDatabaseRepo {
      override def domain = d
    }
  }
  override protected def factRepo(d: DomainId) = {
    val dimRepo = dimensionRepo(d)
    val formulaRepo = new JsonFormulaMapperRepository {
      override val mappers = FudimFormulas.json(dataTypes, dimRepo)
    }
    new DatabaseFactRepo with PlayDatabaseRepo {
      override def domain = DomainRepo.this.get(d).getOrElse(throw new IllegalStateException(s"Domain $d not found"))
      override val databaseCubeRepo = {
        new DatabaseCubeRepo with PlayDatabaseRepo {
          override def dimension(name: String) = dimRepo.get(name)
        }
      }
      override lazy val jsonCubeMapperRepo = new JsonCubeMapperRepository {
        override val mappers = databaseCubeRepo.json ::
          CubeDecorator.json(JsonMappers.decorator, this) ::
          FormulaCube.json(formulaRepo)(domain.cubes) ::
          Nil
      }
    }
  }
}
