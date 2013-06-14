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

  private def values(of: DatabaseDimension): List[String] = DB.withConnection { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim}").on("dim" -> of.id).
      as(scalar[String] *)
  }
  private def addValue(to: DatabaseDimension, v: String) = DB.withConnection { implicit c ⇒
    val max = SQL("select max(nr) from dimension_value where dimension = {dim}").on("dim" -> to.id).single(scalar[Long] ?)
    SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").on("dim" -> to.id, "nr" -> max.getOrElse(0), "val" -> v).executeUpdate
  }

  private[models] val dimension: RowParser[Dimension] = {
    long("dimension.id") ~ str("dimension.name") map {
      case id ~ name ⇒ DatabaseDimension(id, name)
    }
  }
  private[models] def idOf(d: Dimension) = d match {
    case DatabaseDimension(id, _) ⇒ id
    case _ ⇒ throw new IllegalArgumentException(s"Dimension $d.name is not supported by idOf")
  }

  private case class DatabaseDimension(id: Long, name: String) extends Dimension {
    override def values = Dimension.values(this)
    override def add(v: String) = addValue(this, v)
  }
}