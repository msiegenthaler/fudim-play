package models.db

import org.specs2.mutable.Specification
import models._
import models.playbinding.FudimResources
import support._

class DatabaseDimensionRepoSpec extends Specification {
  trait repo extends withModel with withDbVersioner {
    def domain: FudimDomain = new FudimDomain {
      override def id = DomainId(123)
      override def name = "test-domain"
      override def dimensionRepo = repo.this.repo
      override def factRepo = ???
      override def version = ???
    }
    val repo = new DatabaseDimensionRepo with FudimResources {
      override def domain = repo.this.domain.id
      override def versioner = repo.this.versioner
    }
  }
  trait testDim extends repo {
    lazy val dim = execTx { repo.create("Test") }
    override def before = {
      super.before
      dim
    }
  }

  "DatabaseDimensionRepo.create" should {
    "create a new dimension" in new repo {
      val d = execTx { repo.create("Test") }
      repo.get("Test") must beSome(d)
    }
    "throw an IllegalStateException on duplicate name" in new testDim {
      execTx { repo.create("Test") } must throwA[IllegalStateException]
    }
    "give a new version to the dimension" in new testDim {
      val d = execTx { repo.create("Test2") }
      d.version must be > dim.version
    }
  }
  "DatabaseDimensionRepo.remove" should {
    "delete an existing dimension" in new testDim {
      repo.get("Test").isDefined must beTrue
      execTx { repo.remove("Test") }
      repo.get("Test") must beNone
    }
    "do nothing on a non-existing dimension" in new testDim {
      repo.get("non-existing") must beNone
      execTx { repo.remove("non-existing") }
      repo.get("non-existing") must beNone
    }
  }
  "DatabaseDimensionRepo.get" should {
    "return Some() for an existing dimension" in new testDim {
      repo.get("Test") must beSome(dim)
    }
    "return None for a non-existing dimension" in new testDim {
      repo.get("non-existing") must beNone
    }
    "not change the version" in new testDim {
      val d = repo.get("Test").get
      dim.version must_== d.version
    }
  }
  "DatabaseDimensionRepo.all" should {
    "list nothing if no dimensions exist" in new repo {
      repo.all must_== Nil
    }
    "find the only dimension if only one exists" in new testDim {
      repo.all must_== List(dim)
    }
    "list two dimensions if two exist" in new testDim {
      val d2 = execTx { repo.create("Test2") }
      repo.all.toSet must_== Set(dim, d2)
    }
    "not change the version" in new testDim {
      repo.all.head.version must_== dim.version
    }
  }

  "DatabaseDimension.add" should {
    "add a first value to the dimension" in new testDim {
      val c = execTx { dim.add("v1") }
      dim.all.size must_== 1
      dim.all must_== List(c)
      dim.render(c) must_== "v1"
    }
    "when called twice add a two values to the dimension" in new testDim {
      val c1 = execTx { dim.add("v1") }
      val c2 = execTx { dim.add("v2") }
      dim.all.size must_== 2
      dim.all must_== List(c1, c2)
      dim.render(c1) must_== "v1"
      dim.render(c2) must_== "v2"
    }
  }
  "DatabaseDimension.add(after)" should {
    "insert a new value at the beginning" in new testDim {
      val c1 = execTx { dim.add("v1") }
      val c2 = execTx { dim.add("v2") }
      val c3 = execTx { dim.add("v3", None) }
      dim.all must_== List(c3, c1, c2)
    }
    "insert a new value in the middle" in new testDim {
      val c1 = execTx { dim.add("v1") }
      val c2 = execTx { dim.add("v2") }
      val c3 = execTx { dim.add("v3", Some(c1)) }
      dim.all must_== List(c1, c3, c2)
    }
    "insert a new value at the end" in new testDim {
      val c1 = execTx { dim.add("v1") }
      val c2 = execTx { dim.add("v2") }
      val c3 = execTx { dim.add("v3", Some(c2)) }
      dim.all must_== List(c1, c2, c3)
    }
    "increase the version" in new testDim {
      val v0 = dim.version
      val c1 = execTx { dim.add("v1") }
      val v1 = dim.version
      val c2 = execTx { dim.add("v1") }
      val v2 = dim.version
      v1 must be > v0
      v2 must be > v1
    }
    "increase the version when inserting in the middle" in new testDim {
      val v0 = dim.version
      val c1 = execTx { dim.add("v1") }
      val v1 = dim.version
      val c2 = execTx { dim.add("v1") }
      val v2 = dim.version
      val c3 = execTx { dim.add("v3", Some(c2)) }
      val v3 = dim.version
      v2 must be > v1
      v3 must be > v2
    }
  }
}