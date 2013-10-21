package models.db

import org.specs2.mutable.Specification
import domain._
import models._
import base.tx
import cube.Dimension
import support.{ withModel, withDbVersioner }
import models.playbinding.FudimResources
import domain.db.DatabaseCubeDataStoreRepo

class DatabaseFactRepoSpec extends Specification {
  trait repo extends withModel with withDbVersioner {
    def domain: FudimDomain = new FudimDomain {
      override def id = DomainId(123)
      override def name = "test-domain"
      override def dimensionRepo = repo.this.dimensionRepo
      override def factRepo = repo
      override def version = ???
    }
    val dimensionRepo = new DatabaseDimensionRepo with FudimResources {
      override def domain = repo.this.domain.id
    }
    val cubeStore = new DatabaseCubeDataStoreRepo with FudimResources {
      override def dataTypeRepo = FudimDataTypes
      override def dimensionRepo = repo.this.dimensionRepo
      override def storeTypes = playbinding.StoreDataTypes.integer :: Nil
      override def versioner = repo.this.versioner
    }
    val repo = new DatabaseFactRepo with FudimResources {
      override def cubeDataStoreRepo = cubeStore
      private val outer = this
      override def domain = repo.this.domain
      override def jsonFormulaRepo = new JsonFormulaMapperRepository {
        override val mappers = FudimFormulas.json(FudimDataTypes, dimensionRepo)
      }
      override def versioner = repo.this.versioner
    }
  }

  trait testFact extends repo {
    lazy val fact = execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) }
    override def before = {
      super.before
      fact
    }
  }

  "DatabaseFactRepo.createDatabaseBacked" should {
    "create a new fact of type integer" in new repo {
      val fact = execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) }
      fact.name must_== "Test"
      fact.dataType must_== FudimDataTypes.integer
      fact.dimensions must_== Set.empty
      repo.get("Test") must beSome(fact)
    }
    "throw an IllegalStateException on duplicate name" in new repo {
      val fact1 = execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) }
      execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) } must throwA[IllegalStateException]
    }
    "throw an IllegalStateException on duplicate name (mixed)" in new repo {
      val fact1 = execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) }
      execTx { repo.createFormulaBased("Test", FudimDataTypes.integer, FudimFormulas.add(Nil, Nil), Aggregation.none) } must throwA[IllegalStateException]
    }
  }
  "DatabaseFactRepo.createFormulaBased" should {
    "create a new fact of type integer" in new repo {
      val fact = execTx { repo.createFormulaBased("Test", FudimDataTypes.integer, FudimFormulas.add(Nil, Nil), Aggregation.none) }
      fact.name must_== "Test"
      fact.dataType must_== FudimDataTypes.integer
      fact.dimensions must_== Set.empty
      repo.get("Test") must beSome(fact)
    }
    "throw an IllegalStateException on duplicate name" in new repo {
      val fact1 = execTx { repo.createFormulaBased("Test", FudimDataTypes.integer, FudimFormulas.add(Nil, Nil), Aggregation.none) }
      execTx { repo.createFormulaBased("Test", FudimDataTypes.integer, FudimFormulas.add(Nil, Nil), Aggregation.none) } must throwA[IllegalStateException]
    }
    "throw an IllegalStateException on duplicate name (mixed)" in new repo {
      val fact1 = execTx { repo.createFormulaBased("Test", FudimDataTypes.integer, FudimFormulas.add(Nil, Nil), Aggregation.none) }
      execTx { repo.createDatabaseBacked("Test", FudimDataTypes.integer, Set.empty, Aggregation.none) } must throwA[IllegalStateException]
    }
  }

  "DatabaseFactRepo.remove" should {
    "delete a fact" in new testFact {
      repo.get("Test") must beSome(fact)
      execTx { repo.remove("Test") }
      repo.get("Test") must beNone
    }
    "do nothing if the fact does not exist" in new testFact {
      repo.get("non-existing") must beNone
      execTx { repo.remove("non-existing") }
      repo.get("non-existing") must beNone
      repo.get("Test") must beSome(fact)
    }
  }

  "DatabaseFactRepo.all" should {
    "be empty if no facts have been added" in new repo {
      repo.all must beEmpty
    }
    "contain one fact if one fact has been added" in new testFact {
      repo.all must_== List(fact)
    }
  }
  "DatabaseFact.addDimension" should {
    "add a dimension to the fact" in new testFact {
      fact.dimensions.size must_== 0
      val coord = execTx { dimensionRepo.create("TD").add("Xx") }
      execTx { fact.addDimension(coord) }
      repo.get("Test").get.dimensions.size must_== 1
      fact.dimensions.size must_== 1
    }
  }
  "DatabaseFact.removeDimension" should {
    "remove a dimension from the fact" in new testFact {
      val coord = execTx { dimensionRepo.create("TD").add("Xx") }
      execTx { fact.addDimension(coord) }
      repo.get("Test").get.dimensions.size must_== 1
      execTx { fact.removeDimension(coord) }
      repo.get("Test").get.dimensions.size must_== 0
      fact.dimensions.size must_== 0
    }
  }

  "DatabaseFact.version" should {
    "not change when fact is not changed" in new testFact {
      fact.version must_== fact.version
    }
    "not change when fact is reloaded" in new testFact {
      val f = repo.get("Test").get
      fact.version must_== f.version
    }
    "increase in later created facts" in new repo {
      val fact1 = execTx { repo.createDatabaseBacked("Test1", FudimDataTypes.integer, Set.empty, Aggregation.none) }
      val fact2 = execTx { repo.createDatabaseBacked("Test2", FudimDataTypes.integer, Set.empty, Aggregation.none) }
      fact1.version must be < fact2.version
    }
    "be the same for two facts created in the same tx" in new repo {
      val (fact1, fact2) = execTx {
        val fact1 = repo.createDatabaseBacked("Test1", FudimDataTypes.integer, Set.empty, Aggregation.none)
        val fact2 = repo.createDatabaseBacked("Test2", FudimDataTypes.integer, Set.empty, Aggregation.none)
        (fact1, fact2)
      }
      fact1.version must_== fact2.version
    }
    "increase when the aggragation is changed" in new testFact {
      val v1 = fact.version
      execTx { fact.aggregation = Aggregation.sum }
      v1 must be < fact.version
      fact.version must_== repo.get("Test").get.version
    }
    "increase when a dimension is added" in new testFact {
      val v1 = fact.version
      val coord = execTx { dimensionRepo.create("TD").add("Xx") }
      execTx { fact.addDimension(coord) }
      v1 must be < fact.version
      fact.version must_== repo.get("Test").get.version
    }
    "not change when data is edited" in new testFact {
      val coord = execTx { dimensionRepo.create("TD").add("Xx") }
      execTx { fact.addDimension(coord) }
      execTx { fact.editor.get.set(coord, 21) }
      val v1 = fact.version
      execTx { fact.editor.get.set(coord, 22) }
      fact.version must_== v1
    }
  }
}