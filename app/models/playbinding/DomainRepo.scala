package models
package playbinding

import domain._
import db._
import dbcube._

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
    val cdsRepo = new DatabaseCubeDataStoreRepo with PlayDatabaseRepo {
      protected def dimensionRepo = dimRepo
      protected def dataTypeRepo = dataTypes
      protected def storeTypes = StoreDataTypes.all
    }
    new DatabaseFactRepo with PlayDatabaseRepo {
      override def domain = DomainRepo.this.get(d).getOrElse(throw new IllegalStateException(s"Domain $d not found"))
      override protected def jsonFormulaRepo = formulaRepo
      override protected def cubeDataStoreRepo = cdsRepo
    }
  }
}
