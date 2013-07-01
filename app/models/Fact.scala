package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.Connection
import models.cube.ValueCannotBeSetException

/** A fact has values for each coordinate in dimensions. */
sealed trait Fact {
  /** Unique name of the fact. */
  def name: String
  /** Dimensions that fully define the fact. */
  def dimensions: Set[Dimension]

  def get(at: Point): Option[String]
  /** Set the value at the point to value. Throws an ValueCannotBeSetException if not settable. */
  def set(at: Point, value: Option[String]): Unit = throw ValueCannotBeSetException(at)
  final def set(at: Point, value: String): Unit = set(at, Some(value))
  /** Whether the value at the point can be set. */
  def canSet(at: Point): Boolean = false
}

/** Fact that is backed by a database store for all fully defined points. */
sealed trait DatabaseBackedFact extends Fact {
  override def get(at: Point) = {
    Some(at).filter(_.definesExactly(dimensions)).flatMap(FactDatabaseStore.get(this, _))
  }
  override def set(at: Point, value: Option[String]) = {
    if (!canSet(at)) throw ValueCannotBeSetException(at)
    else FactDatabaseStore.set(this, at, value)
  }
  override def canSet(at: Point) = at.definesExactly(dimensions)
}

case class DataFact(name: String, dimensions: Set[Dimension]) extends DatabaseBackedFact

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    SQL("select id from fact where name={name}").on("name" -> name).as(scalar[Long].singleOpt).map { id ⇒
      DataFact(name, dimensionsFor(id))
    }
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    SQL("select id, name from fact").as(long("id") ~ str("name") *).map(_ match {
      case id ~ name ⇒ DataFact(name, dimensionsFor(id))
    })
  }

  private def dimensionsFor(factId: Long)(implicit c: Connection) = {
    SQL("""select d.* from fact_dimension fd
           left outer join dimension d on d.id = fd.dimension
           where fd.fact = {id}""").on("id" -> factId).as(Dimension.dimension *).toSet
  }

  def save(fact: Fact) = DB.withConnection { implicit c ⇒
    val id = SQL("select id from fact where name={name}").on("name" -> fact.name).as(scalar[Long].singleOpt) match {
      case Some(id) ⇒
        SQL("update fact set name={name} where id={id}").on("name" -> fact.name, "id" -> id).executeUpdate
        SQL("delete from fact_dimension where fact={fact}").on("fact" -> id).executeUpdate
        id
      case None ⇒
        SQL("insert into fact(name) values({name})").on("name" -> fact.name).executeInsert().get
    }
    fact.dimensions.foreach { d ⇒
      SQL("insert into fact_dimension(fact, dimension) values({fact},{dim})").on("fact" -> id, "dim" -> Dimension.idOf(d)).executeUpdate
    }
  }
}