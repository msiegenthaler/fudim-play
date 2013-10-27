package models
package db

import anorm._
import anorm.SqlParser._
import base._
import domain._
import support.AnormDb

trait DatabaseDomainRepo extends DomainRepo {
  protected def versioner: Versioner
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
    get(name).map { _ ⇒
      throw new IllegalStateException(s"Domain $name already exists.")
    }.getOrElseTx {
      val version = versioner.version
      val id = DomainId(db.insert(SQL("insert into domain(name, version) values ({name}, {version})").on("name" -> name, "version" -> version.id)).get)
      get(id).getOrElse(throw new IllegalStateException(s"Could not insert domain $name"))
    }
  }
  def remove(domain: Domain) = {
    db.delete(SQL("delete from domain where id={id}").on("id" -> domain.id.id))
  }

  private val domain: RowParser[Domain] = long("id").map(DomainId) ~ str("name") ~ long("version") map {
    case id ~ name ~ versionId ⇒ new DatabaseDomain(id, name, Version(versionId), dimensionRepo(id), factRepo(id))
  }

  protected def dimensionRepo(domain: DomainId): FudimDimensionRepo
  protected def factRepo(domain: DomainId): FactRepo

  private class DatabaseDomain(val id: DomainId, val name: String, val domainVersion: Version,
    val dimensionRepo: FudimDimensionRepo, val factRepo: FactRepo) extends Domain {
    override def version = (List(domainVersion) ++ dimensionRepo.all.map(_.version) ++ factRepo.all.map(_.version)).max
    override def equals(o: Any) = o match {
      case o: DatabaseDomain ⇒ id == o.id
      case _ ⇒ false
    }
    override def hashCode = id.hashCode
    override def toString = s"DatabaseDomain($id, $name)"
  }
}
