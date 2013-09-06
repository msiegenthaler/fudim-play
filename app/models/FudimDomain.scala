package models

import cube._
import domain._
import db._

case class DomainId(id: Long)

trait FudimDomain extends Domain {
  def id: DomainId
  def addDimension(name: String): FudimDimension
  def removeDimension(name: String): Unit

  def addFact(name: String): FudimFact[_]
  def removeFact(name: String): Unit
}

trait DomainRepo {
  def all: List[Domain]
  def get(name: String): Option[Domain]
  def get(id: DomainId): Option[Domain]
  def add(name: String): Domain
  def remove(name: String): Domain
}