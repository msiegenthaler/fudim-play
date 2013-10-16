package models
package playbinding

import db._
import domain._
import domain.db._

object DomainRepo extends DatabaseDomainRepo with FudimResources {
  protected val dataTypes = FudimDataTypes

  object versionRepo extends DatabaseVersionRepo with FudimResources

  object versioner extends Versioner[FudimVersion] {
    override protected def createVersion() = versionRepo.create()
  }

  override protected def dimensionRepo(d: DomainId) = {
    new DatabaseDimensionRepo with FudimResources {
      override def domain = d
    }
  }
  override protected def factRepo(d: DomainId) = {
    val dimRepo = dimensionRepo(d)
    val formulaRepo = new JsonFormulaMapperRepository {
      override val mappers = FudimFormulas.json(dataTypes, dimRepo)
    }
    val cdsRepo = new DatabaseCubeDataStoreRepo[FudimVersion] with FudimResources {
      protected def dimensionRepo = dimRepo
      protected def dataTypeRepo = dataTypes
      protected def storeTypes = StoreDataTypes.all
      protected def versioner = DomainRepo.versioner
      protected def versionFromId(id: Long) = FudimVersion(id)
    }
    new DatabaseFactRepo with FudimResources {
      override def domain = DomainRepo.this.get(d).getOrElse(throw new IllegalStateException(s"Domain $d not found"))
      override protected def jsonFormulaRepo = formulaRepo
      override protected def cubeDataStoreRepo = cdsRepo
    }
  }
}
