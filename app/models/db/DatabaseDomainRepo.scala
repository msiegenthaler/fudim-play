package models
package db

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import domain._

trait DatabaseDomainRepo extends FudimDomainRepo with DatabaseRepo {
  override def all = withConnection { implicit c ⇒
    SQL("select * from domain").as(domain *)
  }
  def get(name: String) = withConnection { implicit c ⇒
    SQL("select * from domain where name={name}").on("name" -> name).as(domain.singleOpt)
  }
  def get(id: DomainId) = withConnection { implicit c ⇒
    SQL("select * from domain where id={id}").on("id" -> id.id).as(domain.singleOpt)
  }
  def create(name: String) = withConnection { implicit c ⇒
    SQL("insert into domain(name) values ({name})").on("name" -> name).executeInsert()
    get(name).getOrElse(throw new IllegalStateException(s"Could not insert domain $name"))
  }
  def remove(id: FudimDomain) = withConnection { implicit c ⇒
    SQL("delete from domain where id={id}").on("id" -> id.id).executeUpdate
  }

  private val domain: RowParser[FudimDomain] = long("id").map(DomainId(_)) ~ str("name") map {
    case id ~ name ⇒ DatabaseDomain(id, name, dimensionRepo(id), factRepo(id))
  }

  protected def dimensionRepo(domain: DomainId): FudimDimensionRepo
  protected def factRepo(domain: DomainId): FudimFactRepo

  private case class DatabaseDomain(id: DomainId, name: String, dimensionRepo: FudimDimensionRepo, factRepo: FudimFactRepo) extends FudimDomain {
    def dimensions = dimensionRepo.all.toSet
    def dimension(name: String) = dimensionRepo.get(name)

    def facts = factRepo.all.toSet
    def fact(name: String) = factRepo.get(name)
  }
}