package models

import base._
import cube._
import domain._

case class DomainId(id: Long)

trait Domain {
  def id: DomainId

  def name: String

  /** Version of the structure of the domain (data is ignored). */
  def version: Version

  def dimensions: FudimDimensionRepo
  def facts: FactRepo
  def cubes: Cubes = DomainCubes(Domain.this)
}

private case class DomainCubes(domain: Domain) extends Cubes {
  override def refs = domain.facts.all.map(f ⇒ CubeRef(f.name, f.dataType)).toSet
  override def get[T](ref: CubeRef[T]) = {
    domain.facts.get(ref.name).filter(_.dataType == ref.dataType).map(_.data).map({ case c: VersionedCube[T] ⇒ c })
  }
}

trait DomainRepo {
  def all: List[Domain]
  def get(name: String): Option[Domain]
  def get(id: DomainId): Option[Domain]

  def create(name: String): Domain @tx
  def remove(id: Domain): Unit @tx
}
