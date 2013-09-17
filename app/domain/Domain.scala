package domain

import cube._

trait Domain {
  def name: String

  def dimensions: Set[Dimension]
  def dimension(name: String): Option[Dimension]

  def facts: Set[Fact[_]]
  def fact(name: String): Option[Fact[_]]
}