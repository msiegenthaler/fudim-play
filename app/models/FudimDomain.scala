package models

import base._
import domain._

case class DomainId(id: Long)

trait FudimDomain extends Domain {
  def id: DomainId
  def version: Version

  def dimensionRepo: FudimDimensionRepo
  def factRepo: FudimFactRepo

  def dimensions = dimensionRepo.all.toSet
  def dimension(name: String) = dimensionRepo.get(name)
  def facts = factRepo.all.toSet
  def fact(name: String) = factRepo.get(name)
}

trait FudimDomainRepo {
  def all: List[FudimDomain]
  def get(name: String): Option[FudimDomain]
  def get(id: DomainId): Option[FudimDomain]

  def create(name: String): FudimDomain @tx
  def remove(id: FudimDomain): Unit @tx
}
