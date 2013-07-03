package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Coordinate(val id: Long) extends AnyVal

trait Dimension {
  /** Unique name of the dimension. */
  def name: String

  /** Values of this dimensions (ordered). */
  def all: Seq[Coordinate]
  /** Point on this dimension (one-dimensional). */
  def points: Seq[Point] = all.map(Point(this, _))

  def values: Seq[(Coordinate, String)]

  /** String value of the coordinate. */
  def render(c: Coordinate): String

  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate

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
  def delete(name: String) = DB.withConnection { implicit c ⇒
    get(name).foreach { d ⇒
      val id = idOf(d)
      SQL("delete from dimension where id = {id}").on("id" -> id).executeUpdate
      SQL("delete from dimension_value where dimension = {dim}").on("dim" -> id).executeUpdate
    }
  }

  private def coordinates(of: DatabaseDimension): List[Coordinate] = DB.withConnection { implicit c ⇒
    SQL("select id from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id").map(e ⇒ Coordinate(e)) *)
  }
  private def values(of: DatabaseDimension): List[(Coordinate, String)] = DB.withConnection { implicit c ⇒
    SQL("select id, content from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id") ~ str("content") map { case id ~ content ⇒ (Coordinate(id), content) } *)
  }
  private def render(of: DatabaseDimension, at: Coordinate): String = DB.withConnection { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim} and id = {id}").on("dim" -> of.id, "id" -> at.id).as(scalar[String] single)
  }
  private def addValue(to: DatabaseDimension, v: String) = DB.withConnection { implicit c ⇒
    val max = SQL("select max(nr) from dimension_value where dimension = {dim}").on("dim" -> to.id).single(scalar[Long] ?)
    val id = SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
      on("dim" -> to.id, "nr" -> max.map(_ + 1).getOrElse(0), "val" -> v).executeInsert().get
    Coordinate(id)
  }
  private def addValue(to: DatabaseDimension, v: String, after: Option[Coordinate]): Coordinate = DB.withConnection { implicit c ⇒
    SQL("select nr from dimension_value where id = {id}").on("id" -> to.id).as(long("nr") singleOpt) match {
      case Some(nr) ⇒
        SQL("update dimension_value set nr = nr + 1 where dimension = {dim} and nr >= {nr}").on("dim" -> to.id, "nr" -> nr).executeUpdate
        val id = SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
          on("dim" -> to.id, "nr" -> nr, "val" -> v).executeInsert().get
        Coordinate(id)
      case None ⇒ addValue(to, v)
    }
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
    override def all = Dimension.coordinates(this)
    override def values = Dimension.values(this)
    override def render(c: Coordinate) = Dimension.render(this, c)
    override def add(v: String) = addValue(this, v)
    override def add(v: String, after: Option[Coordinate]) = addValue(this, v, after)
  }
}