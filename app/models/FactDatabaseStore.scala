package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.Connection

private object FactDatabaseStore {
  def get(fact: Fact, at: Point): Option[String] = DB.withConnection { implicit c ⇒
    if (!at.defines(fact.dimensions)) None
    else idAndValue(fact, at).map(_._2)
  }

  def set(fact: Fact, at: Point, value: Option[String]): Unit = value match {
    case Some(value) ⇒ set(fact, at, value)
    case None ⇒ remove(fact, at)
  }
  def set(fact: Fact, at: Point, value: String): Unit = DB.withConnection { implicit c ⇒
    if (!at.defines(fact.dimensions)) throw new IllegalArgumentException("at not fully defined")

    idAndValue(fact, at) match {
      case Some((_, `value`)) ⇒ () //already correct
      case Some((id, _)) ⇒ // point already exists, change the value
        SQL("update factStore_value set value = {value} where id = {id}").on("value" -> value, "id" -> id).executeUpdate
      case None ⇒ // need to create the point
        val factId = SQL("select id from fact where name={name}").on("name" -> fact.name).as(int("id").singleOpt).
          getOrElse(throw new IllegalArgumentException(s"Fact ${fact.name} not in database"))
        val valueId = SQL("insert into factStore_value(fact, value) values({fact}, {value})").on("fact" -> factId, "value" -> value).executeInsert().get
        at.values.foreach { v ⇒
          SQL("insert into factStore_dimension(id, dimension, value) values({id}, {dim}, {dimv})").
            on("id" -> valueId, "dim" -> Dimension.idOf(v._1), "dimv" -> v._2).executeInsert()
        }
    }
  }
  def remove(fact: Fact, at: Point): Boolean = DB.withConnection { implicit c ⇒
    if (!at.defines(fact.dimensions)) throw new IllegalArgumentException("at not fully defined")
    idAndValue(fact, at) match {
      case None ⇒ false //does not exist, so nothing to delete
      case Some((id, _)) ⇒
        SQL("delete from factStore_value where id={id}").on("id" -> id).executeUpdate
        SQL("delete from factStore_dimension where id={id}").on("id" -> id).executeUpdate
        true
    }
  }

  private def idAndValue(fact: Fact, at: Point)(implicit c: Connection): Option[(Int, String)] = {
    val sql = "select fv.id as id, fv.value as value from factStore_value fv inner join fact f on f.id = fv.fact"
    val (psql, pon) = pointSelector("fv", at)
    val mapper = int("id") ~ str("value") map { case a ~ b ⇒ (a, b) }
    val pon2: Map[Any, ParameterValue[_]] = pon + ("fact" -> fact.name)
    SQL(sql + " " + psql + " where f.name={fact}").on(pon2.toList: _*).as(mapper.singleOpt)
  }

  private def pointSelector(fv: String, at: Point) = {
    val x = at.values.zipWithIndex.map { v ⇒
      val ((d, value), i) = v
      val tn = "d" + i
      val sql = s"inner join factStore_dimension $tn on $tn.id=$fv.id and $tn.dimension={d$tn} and $tn.value={v$tn}"
      (sql, Map[Any, ParameterValue[_]]("d" + tn -> Dimension.idOf(d), "v" + tn -> value))
    }
    val sql = x.map(_._1).mkString(" ")
    val on = x.map(_._2).reduce(_ ++ _)
    (sql, on)
  }

}