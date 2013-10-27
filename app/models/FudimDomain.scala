package models

import base._
import cube._
import domain._

case class DomainId(id: Long)

trait FudimDomain {
  def id: DomainId

  def name: String

  /** Version of the structure of the domain (data is ignored). */
  def version: Version

  def dimensionRepo: FudimDimensionRepo
  def dimensions = dimensionRepo.all.toSet
  def dimension(name: String) = dimensionRepo.get(name)

  def factRepo: FudimFactRepo
  def facts = factRepo.all.toSet
  def fact(name: String) = factRepo.get(name)

  def cubes: Cubes = DomainCubes(this)
}

private case class DomainCubes(domain: FudimDomain) extends Cubes {
  override def refs = domain.facts.map(f ⇒ CubeRef(f.name, f.dataType))
  override def get[T](ref: CubeRef[T]) = {
    domain.fact(ref.name).filter(_.dataType == ref.dataType).map(_.data.asInstanceOf[Cube[T]])
  }
}

trait FudimDomainRepo {
  def all: List[FudimDomain]
  def get(name: String): Option[FudimDomain]
  def get(id: DomainId): Option[FudimDomain]

  def create(name: String): FudimDomain @tx
  def remove(id: FudimDomain): Unit @tx
}
