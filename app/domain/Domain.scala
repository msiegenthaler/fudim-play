package domain

import cube._

trait Domain {
  def name: String

  def dimensions: Set[Dimension]
  def dimension(name: String): Option[Dimension]

  def facts: Set[Fact[_]]
  def fact(name: String): Option[Fact[_]]

  def cubes: Cubes = DomainCubes(this)
}

private case class DomainCubes(domain: Domain) extends Cubes {
  override def refs = domain.facts.map(f ⇒ CubeRef(f.name, f.dataType))
  override def get[T](ref: CubeRef[T]) = {
    domain.fact(ref.name).filter(_.dataType == ref.dataType).map(_.data.asInstanceOf[Cube[T]])
  }
}
