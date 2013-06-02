package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Fact(name: String, dimensions: Set[Dimension])

object Fact {
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    val vs = SQL("select name, dimension from fact f left outer join fact_dimension fd on f.id = fd.fact").
      as(get[String]("name") ~ get[Option[String]]("dimension") *)
    vs.groupBy(_._1).map(v ⇒ Fact(v._1, v._2.map(_._2).flatten.map(Dimension.apply).toSet))
  }
  def find(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    val vs = SQL("select name, dimension from fact f left outer join fact_dimension fd on f.id = fd.fact where name = {name}").
      on("name" -> name).
      as(get[String]("name") ~ get[Option[String]]("dimension") *)
    if (vs.length == 0) None
    else Some(Fact(name, vs.map(_._2).flatten.map(Dimension.apply).toSet))
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