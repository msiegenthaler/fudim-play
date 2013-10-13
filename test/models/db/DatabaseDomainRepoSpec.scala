package models.db

import org.specs2.mutable.Specification

class DatabaseDomainRepoSpec extends Specification {
  "DatabaseDomainRepo.create" should {
    "create a new domain" in pending
    "throw an exception on duplicate name" in pending
  }
  "DatabaseDomainRepo.remove" should {
    "delete the domain" in pending
    "do nothing if the domain does not exist" in pending
  }
  "DatabaseDomainRepo.all" should {
    "be empty if no domains have been created" in pending
    "list one domain if one has been created" in pending
   "list all domains" in pending
  }
  "DatabaseDomainRepo.get" should {
    "return None for non-existing domain" in pending
    "return Some(domain) for an existing domain" in pending
  }
}
