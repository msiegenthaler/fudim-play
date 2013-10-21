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
    }
  }

  "DatabaseDimensionRepo.create" should {
    "create a new dimension" in pending
    "throw an IllegalStateException on duplicate name" in pending
  }
  "DatabaseDimensionRepo.remove" should {
    "delete an existing dimension" in pending
    "do nothing on a non-existing dimension" in pending
  }
  "DatabaseDimensionRepo.get" should {
    "return Some() for an existing dimension" in pending
    "return None for a non-existing dimension" in pending
  }
  "DatabaseDimensionRepo.all" should {
    "list nothing if no dimensions exist" in pending
    "find the only dimension if only one exists" in pending
    "list two dimensions if two exist" in pending
  }

  "DatabaseDimension.add" should {
    "add a first value to the dimension" in pending
    "add a new value to the dimension" in pending
    "insert a new value at the beginning" in pending
    "insert a new value in the middle" in pending
    "insert a new value at the end" in pending
  }
}