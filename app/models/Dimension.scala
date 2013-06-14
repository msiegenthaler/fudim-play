package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

trait Dimension {
  /** Unique name of the dimension. */
  def name: String

  /** Values of this dimensions (ordered). */
  def values: Seq[String]
  /** Add a value to the dimension (at last index). */
  def add(value: String): Unit

  override def toString = name
}

object Dimension {
  def get(name: String): Option[Dimension] = DB.withConnection { implicit c ⇒
    SQL("select * from dimension where name={name}").on("name" -> name).as(dimension.singleOpt)
  }

  def all(): List[Dimension] = DB.withConnection { implicit c ⇒
    SQL("select * from dimension").as(dimension *)
  }
  def create(name: String) = DB.withConnection { implicit c ⇒
    SQL("insert into dimension(name) values({name})").on("name" -> name).executeUpdate
    get(name).getOrElse(throw new IllegalStateException(s"Insert of dimension $name failed"))
  }

  private def values(of: Dimension): List[String] = DB.withConnection { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim}").on("dim" -> of).
      as(scalar[String] *)
  }
  private def addValue(to: Dimension, v: String) = DB.withConnection { implicit c ⇒
    val max = SQL("select max(nr) from dimension_value where dimension = {dim}").on("dim" -> to.name).single(scalar[Long] ?)
    SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").on("dim" -> to, "nr" -> max.getOrElse(0), "val" -> v).executeUpdate
  }

  private val dimension: RowParser[Dimension] = {
    str("name") map {
      case name ⇒ DatabaseDimension(name)
    }
  }

  private case class DatabaseDimension(name: String) extends Dimension {
    override def values = Dimension.values(this)
    override def add(v: String) = addValue(this, v)
  }
}