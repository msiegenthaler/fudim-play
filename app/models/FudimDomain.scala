package models

import cube._
import domain._
import db._

case class DomainId(id: Long)

trait FudimDomain extends Domain {
  def id: DomainId

  def dimensionRepo: FudimDimensionRepo
  def factRepo: FudimFactRepo
}

trait FudimDomainRepo {
  def all: List[FudimDomain]
  def get(name: String): Option[FudimDomain]
  def get(id: DomainId): Option[FudimDomain]

  def create(name: String): FudimDomain
  def remove(id: FudimDomain): Unit
}