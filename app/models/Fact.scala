package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

sealed trait Fact {
  /** Unique name of the fact. */
  def name: String
  def dimensions: Set[Dimension]

  def get(at: Point): Option[String]
  /** Set the value at the point to value. Throws an ValueCannotBeSetException if not settable. */
  def set(at: Point, value: Option[String]): Unit = throw ValueCannotBeSetException(this, at)
  final def set(at: Point, value: String): Unit = set(at, Some(value))
  /** Whether the value at the point can be set. */
  def canSet(at: Point): Boolean = false
}

/** Fact that is backed by a database store for all fully defined points. */
sealed trait DatabaseBackedFact extends Fact {
  override def get(at: Point) = {
    Some(at).filter(_.defines(dimensions)).flatMap(FactDatabaseStore.get(this, _))
  }
  override def set(at: Point, value: Option[String]) = {
    if (!canSet(at)) throw ValueCannotBeSetException(this, at)
    else FactDatabaseStore.set(this, at, value)
  }
  override def canSet(at: Point) = at.defines(dimensions)
}

case class DataFact(name: String, dimensions: Set[Dimension]) extends DatabaseBackedFact

case class ValueCannotBeSetException(fact: Fact, at: Point) extends RuntimeException(s"Cannot set value of $fact.name at $at")

object Fact {
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    val vs = SQL("select name, dimension from fact f left outer join fact_dimension fd on f.id = fd.fact").
      as(get[String]("name") ~ get[Option[String]]("dimension") *)
    vs.groupBy(_._1).map(v ⇒ DataFact(v._1, v._2.map(_._2).flatten.map(Dimension.apply).toSet))
  }
  def find(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    val vs = SQL("select name, dimension from fact f left outer join fact_dimension fd on f.id = fd.fact where name = {name}").
      on("name" -> name).
      as(get[String]("name") ~ get[Option[String]]("dimension") *)
    if (vs.length == 0) None
    else Some(DataFact(name, vs.map(_._2).flatten.map(Dimension.apply).toSet))
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
      SQL("insert into fact_dimension(fact, dimension) values ({fact}, {dim})").on("fact" -> id, "dim" -> d.name).executeUpdate
    }
  }
}