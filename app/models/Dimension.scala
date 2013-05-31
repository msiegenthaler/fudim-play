package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Dimension(name: String)

object Dimension {
  def all(): List[Dimension] = DB.withConnection { implicit c ⇒
    SQL("select * from dimension").as(dimension *)
  }
  
  def create(name: String) = DB.withConnection { implicit c =>
    SQL("insert into dimension(name) values({name})").on("name" -> name).executeUpdate
	}
  
  val dimension = {
    get[String]("name") map {
      case name ⇒ Dimension(name)
    }
  }
}