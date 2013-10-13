package models
package db

import anorm._
import anorm.SqlParser._
import base._
import support.AnormDb

trait DatabaseDomainRepo extends FudimDomainRepo {
  protected def database: SqlDatabase
  protected val db = new AnormDb(database)

  override def all = {
    db.notx.select(SQL("select * from domain"), domain *)
  }
  def get(name: String) = {
    db.notx.select(SQL("select * from domain where name={name}").on("name" -> name), domain.singleOpt)
  }
  def get(id: DomainId) = {
    db.notx.select(SQL("select * from domain where id={id}").on("id" -> id.id), domain.singleOpt)
  }
  def create(name: String) = {
    db.insert(SQL("insert into domain(name) values ({name})").on("name" -> name))
    get(name).getOrElse(throw new IllegalStateException(s"Could not insert domain $name"))
  }
  def remove(id: FudimDomain) = {
    db.delete(SQL("delete from domain where id={id}").on("id" -> id.id))
  }

  private val domain: RowParser[FudimDomain] = long("id").map(DomainId) ~ str("name") map {
    case id ~ name ⇒ new DatabaseDomain(id, name, dimensionRepo(id), factRepo(id))
  }

  protected def dimensionRepo(domain: DomainId): FudimDimensionRepo
  protected def factRepo(domain: DomainId): FudimFactRepo

  private class DatabaseDomain(val id: DomainId, val name: String,
                               val dimensionRepo: FudimDimensionRepo, val factRepo: FudimFactRepo) extends FudimDomain {
    override def equals(o: Any) = o match {
      case o: DatabaseDomain => id == o.id
      case _ => false
    }
    override def hashCode = id.hashCode
    override def toString = s"DatabaseDomain($id, $name)"
  }
}
