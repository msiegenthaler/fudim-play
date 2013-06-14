package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Dimension(name: String) {
  def values: Iterable[String] = Dimension.values(this)
  def add(value: String): Unit = Dimension.addValue(this, value)
}

object Dimension {
  def all(): List[Dimension] = DB.withConnection { implicit c ⇒
    SQL("select * from dimension").as(dimension *)
  }
  def create(name: String) = DB.withConnection { implicit c ⇒
    SQL("insert into dimension(name) values({name})").on("name" -> name).executeUpdate
  }

  private def values(of: Dimension): List[String] = DB.withConnection { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim}").on("dim" -> of).
      as(get[String]("content") *)
  }
  private def addValue(to: Dimension, v: String) = DB.withConnection { implicit c ⇒
    val max = SQL("select max(nr) as nr from dimension_value where dimension = {dim}").on("dim" -> to.name).single(get[Long]("nr") ?)
    SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").on("dim" -> to, "nr" -> max.getOrElse(0), "val" -> v).executeUpdate
  }

  private val dimension = {
    get[String]("name") map {
      case name ⇒ Dimension(name)
    }
  }
}