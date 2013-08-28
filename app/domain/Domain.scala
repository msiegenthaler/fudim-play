package domain

import cube._

trait Domain {
  def name: String

  def dimensions: Set[Dimension]
  def facts: Set[Fact[_]]
}