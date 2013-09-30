package models
package db

import anorm._
import anorm.SqlParser._
import base._

trait DatabaseDomainRepo extends FudimDomainRepo {
  protected def db: SqlDatabase

  override def all = db.readOnly { implicit c ⇒
    SQL("select * from domain").as(domain *)
  }
  def get(name: String) = db.readOnly { implicit c ⇒
    SQL("select * from domain where name={name}").on("name" -> name).as(domain.singleOpt)
  }
  def get(id: DomainId) = db.readOnly { implicit c ⇒
    SQL("select * from domain where id={id}").on("id" -> id.id).as(domain.singleOpt)
  }
  def create(name: String) = db.transaction { implicit c ⇒
    SQL("insert into domain(name) values ({name})").on("name" -> name).executeInsert()
    get(name).getOrElse(throw new IllegalStateException(s"Could not insert domain $name"))
  }
  def remove(id: FudimDomain) = db.transaction { implicit c ⇒
    SQL("delete from domain where id={id}").on("id" -> id.id).executeUpdate
  }

  private val domain: RowParser[FudimDomain] = long("id").map(DomainId(_)) ~ str("name") map {
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
