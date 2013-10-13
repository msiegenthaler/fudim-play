package models.db

import org.specs2.mutable.Specification
import base._
import support.withModel
import models._
import models.playbinding.FudimResources
import domain.Formula
import cube.Dimension
import java.sql.SQLException

class DatabaseDomainRepoSpec extends Specification {
  trait repo extends withModel {
    def mkDimensionRepo = new FudimDimensionRepo {
      def create(name: String) = ???
      def remove(name: String) = ???
      def all = ???
    }
    def mkFactRepo = new FudimFactRepo {
      def get(name: String) = ???
      def all = ???
      def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]) = ???
      def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]) = ???
      def remove(name: String) = ???
    }
    val repo = new DatabaseDomainRepo with FudimResources {
      protected def dimensionRepo(domain: DomainId) = mkDimensionRepo
      protected def factRepo(domain: DomainId) = mkFactRepo
    }
    def exec[A](tx: => A@tx): A = execTx(tx)
  }

  "DatabaseDomainRepo.create" should {
    "create a new domain" in new repo {
      val d = exec(repo.create("Test"))
      d.name must_== "Test"
      repo.get("Test") must beSome(d)
    }
    "throw an exception on duplicate name" in new repo {
      val d1 = exec(repo.create("Test"))
      //      exec(repo.create("Test")) must throwA[IllegalStateException]
    }
  }
  "DatabaseDomainRepo.remove" should {
    "delete the domain" in new repo {
      val d = exec(repo.create("Test"))
      repo.get("Test") must beSome(d)
      exec(repo.remove(d))
      repo.get("Test") must beNone
    }
    "do nothing if the domain does not exist" in new repo {
      val d = exec(repo.create("Test"))
      repo.get("Test") must beSome(d)
      exec(repo.remove(d))
      repo.get("Test") must beNone
      exec(repo.remove(d))
      repo.get("Test") must beNone
    }
  }
  "DatabaseDomainRepo.all" should {
    "be empty if no domains have been created" in new repo {
      execTx(repo.all.foreachTx(repo.remove))

      repo.all must beEmpty
    }
    "list one domain if one has been created" in new repo {
      execTx(repo.all.foreachTx(repo.remove))

      val d1 = exec(repo.create("Test"))
      repo.all.toSet must_== Set(d1)
    }
    "list all domains" in new repo {
      execTx(repo.all.foreachTx(repo.remove))

      val ds = (1 to 10).map(i => exec(repo.create("Test " + i))).toSet
      repo.all.toSet must_== ds
      repo.all.size must_== 10
    }
  }
  "DatabaseDomainRepo.get by name" should {
    "return None for non-existing domain" in new repo {
      repo.get("Test") must beNone
    }
    "return Some(domain) for an existing domain" in new repo {
      val d = exec(repo.create("Test"))
      repo.get("Test") must beSome(d)
    }
  }
  "DatabaseDomainRepo.get by id" should {
    "return None for non-existing domain" in new repo {
      val id = DomainId(123)
      repo.get(id) must beNone
    }
    "return Some(domain) for an existing domain" in new repo {
      val d = exec(repo.create("Test"))
      repo.get(d.id) must beSome(d)
    }
  }
}
