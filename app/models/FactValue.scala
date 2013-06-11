package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.Connection

object FactValue {

  def get(fact: Fact, at: Point): Option[String] = DB.withConnection { implicit c ⇒
    if (!at.defines(fact.dimensions)) None
    else idAndValue(fact, at).map(_._2)
  }

  def set(fact: Fact, at: Point, value: String): Unit = DB.withConnection { implicit c ⇒
    if (!at.defines(fact.dimensions)) throw new IllegalArgumentException("at not fully defined")

    idAndValue(fact, at) match {
      case Some((_, `value`)) ⇒ () //already correct
      case Some((id, _)) ⇒ // point already exists, change the value
        SQL("update factValue set value = {value} where id = {id}").on("value" -> value, "id" -> id).executeUpdate
      case None ⇒ // need to create the point
        val factId = SQL("select id from fact where name={name}").on("name" -> fact.name).as(int("id").singleOpt).
          getOrElse(throw new IllegalArgumentException(s"Fact ${fact.name} not in database"))
        val valueId = SQL("insert into factValue(fact, value) values({fact}, {value})").on("fact" -> factId, "value" -> value).executeInsert().get
        at.values.foreach { v ⇒
          SQL("insert into factValue_dimension(factValue, dimension, value) values({id}, {dim}, {dimv})").
            on("id" -> valueId, "dim" -> v._1.name, "dimv" -> v._2).executeInsert()
        }
    }
  }

  private def idAndValue(fact: Fact, at: Point)(implicit c: Connection): Option[(Int, String)] = {
    val sql = "select fv.id as id, fv.value as value from factValue fv inner join fact f on f.id = fv.fact"
    val (psql, pon) = pointSelector("fv", at)
    val mapper = int("id") ~ str("value") map { case a ~ b ⇒ (a, b) }
    val pon2: Map[Any, ParameterValue[_]] = pon + ("fact" -> fact.name)
    SQL(sql + " " + psql+" where f.name={fact}").on(pon2.toList: _*).as(mapper.singleOpt)
  }

  private def pointSelector(fv: String, at: Point) = {
    val x = at.values.zipWithIndex.map { v ⇒
      val ((d, value), i) = v
      val tn = "d" + i
      val sql = s"inner join factValue_dimension $tn on $tn.factValue=$fv.id and $tn.dimension={d$tn} and $tn.value={v$tn}"
      (sql, Map[Any, ParameterValue[_]]("d" + tn -> d.name, "v" + tn -> value))
    }
    val sql = x.map(_._1).mkString(" ")
    val on = x.map(_._2).reduce(_ ++ _)
    (sql, on)
  }

}