package models.db

import anorm._
import anorm.SqlParser._
import util.control.Exception._
import cube._
import models.{ DomainId, FudimDimension, FudimDimensionRepo }

trait DatabaseDimensionRepo extends FudimDimensionRepo with CoordinateFactory with DatabaseRepo {
  def domain: DomainId

  override def get(name: String) = withConnection { implicit c ⇒
    SQL("select * from dimension where domain={domain} and name={name}").on("domain" -> domain.id, "name" -> name).as(dimension.singleOpt)
  }

  override def all = withConnection { implicit c ⇒
    SQL("select * from dimension where domain={domain}").on("domain" -> domain.id).as(dimension *)
  }
  def create(name: String) = withConnection { implicit c ⇒
    SQL("insert into dimension(domain, name) values({domain}, {name})").on("domain" -> domain.id, "name" -> name).executeUpdate
    get(name).getOrElse(throw new IllegalStateException(s"Insert of dimension $name failed"))
  }
  def remove(name: String) = withConnection { implicit c ⇒
    get(name).foreach { d ⇒
      val id = idOf(d)
      SQL("delete from dimension where id = {id}").on("id" -> id).executeUpdate
      SQL("delete from dimension_value where dimension = {id}").on("id" -> id).executeUpdate
    }
  }

  private def coordinates(of: DatabaseDimension): List[Coordinate] = withConnection { implicit c ⇒
    SQL("select id from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id").map(e ⇒ coordinate(of, e)) *)
  }
  private def values(of: DatabaseDimension): List[(Coordinate, String)] = withConnection { implicit c ⇒
    SQL("select id, content from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id") ~ str("content") map { case id ~ content ⇒ (coordinate(of, id), content) } *)
  }
  private def render(of: DatabaseDimension, at: Coordinate): String = withConnection { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim} and id = {id}").on("dim" -> of.id, "id" -> at.id).as(scalar[String] single)
  }
  private def addValue(to: DatabaseDimension, v: String) = withConnection { implicit c ⇒
    val max = SQL("select max(nr) from dimension_value where dimension = {dim}").on("dim" -> to.id).single(scalar[Long] ?)
    val id = SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
      on("dim" -> to.id, "nr" -> max.map(_ + 1).getOrElse(0), "val" -> v).executeInsert().get
    coordinate(to, id)
  }
  private def addValue(to: DatabaseDimension, v: String, after: Option[Coordinate]): Coordinate = withConnection { implicit c ⇒
    SQL("select nr from dimension_value where id = {id}").on("id" -> to.id).as(long("nr") singleOpt) match {
      case Some(nr) ⇒
        SQL("update dimension_value set nr = nr + 1 where dimension = {dim} and nr >= {nr}").on("dim" -> to.id, "nr" -> nr).executeUpdate
        val id = SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
          on("dim" -> to.id, "nr" -> nr, "val" -> v).executeInsert().get
        coordinate(to, id)
      case None ⇒ addValue(to, v)
    }
  }

  private val dimension: RowParser[FudimDimension] = {
    long("dimension.id") ~ str("dimension.name") map {
      case id ~ name ⇒ DatabaseDimension(id, name)
    }
  }
  private def idOf(d: Dimension) = d match {
    case DatabaseDimension(id, _) ⇒ id
    case _ ⇒ throw new IllegalArgumentException(s"Dimension $d.name is not supported by idOf")
  }

  private case class DatabaseDimension(id: Long, name: String) extends FudimDimension {
    override def all = coordinates(this)
    override def values = DatabaseDimensionRepo.this.values(this)
    override def render(c: Coordinate) = DatabaseDimensionRepo.this.render(this, c)
    override def add(v: String) = addValue(this, v)
    override def add(v: String, after: Option[Coordinate]) = addValue(this, v, after)
  }
}