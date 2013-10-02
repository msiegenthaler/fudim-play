package models
package db

import anorm._
import anorm.SqlParser._
import base._
import cube._
import support.AnormDb

trait DatabaseDimensionRepo extends FudimDimensionRepo with CoordinateFactory {
  protected def db: SqlDatabase
  protected val Db = new AnormDb(db)
  def domain: DomainId

  override def get(name: String) = db.readOnly { implicit c ⇒
    SQL("select * from dimension where domain={domain} and name={name}").on("domain" -> domain.id, "name" -> name).as(dimension.singleOpt)
  }

  override def all = db.readOnly { implicit c ⇒
    SQL("select * from dimension where domain={domain}").on("domain" -> domain.id).as(dimension *)
  }
  def create(name: String) = {
    Db.insert(SQL("insert into dimension(domain, name) values({domain}, {name})").on("domain" -> domain.id, "name" -> name))
    get(name).getOrElse(throw new IllegalStateException(s"Insert of dimension $name failed")).tx
  }
  def remove(name: String) = {
    get(name).map(idOf).mapTx { id =>
      Db.update(SQL("delete from dimension where id = {id}").on("id" -> id))
      Db.update(SQL("delete from dimension_value where dimension = {id}").on("id" -> id))
    }.getOrElse(())
  }

  private def coordinates(of: DatabaseDimension): List[Coordinate] = db.readOnly { implicit c ⇒
    SQL("select id from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id").map(e ⇒ coordinate(of, e)) *)
  }
  private def values(of: DatabaseDimension): List[(Coordinate, String)] = db.readOnly { implicit c ⇒
    SQL("select id, content from dimension_value where dimension = {dim} order by nr").on("dim" -> of.id).
      as(long("id") ~ str("content") map {
      case id ~ content ⇒ (coordinate(of, id), content)
    } *)
  }
  private def render(of: DatabaseDimension, at: Coordinate): String = db.readOnly { implicit c ⇒
    SQL("select content from dimension_value where dimension = {dim} and id = {id}").on("dim" -> of.id, "id" -> at.id).as(scalar[String] single)
  }

  private def addValue(to: DatabaseDimension, v: String): Coordinate@tx = {
    val max = Db.single(SQL("select max(nr) from dimension_value where dimension = {dim}").on("dim" -> to.id), scalar[Long] ?)
    val id = Db.insert(SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
      on("dim" -> to.id, "nr" -> max.map(_ + 1).getOrElse(0), "val" -> v)).get
    coordinate(to, id)
  }
  private def addValue(to: DatabaseDimension, v: String, after: Option[Coordinate]): Coordinate@tx = {
    if (!after.isDefined) addValue(to, v)
    else {
      val nrOfAfter = Db.select(SQL("select max(nr) from dimension_value where id = {id}").on("id" -> after.get.id), long("nr") singleOpt)
      val nr = nrOfAfter.map { nr =>
        Db.update(SQL("update dimension_value set nr = nr + 1 where dimension = {dim} and nr > {nr}").on("dim" -> to.id, "nr" -> nr)).transaction
      }.getOrElse(Transaction.pure(0)).tx
      val id = Db.insert(SQL("insert into dimension_value(dimension, nr, content) values({dim}, {nr}, {val})").
        on("dim" -> to.id, "nr" -> nr, "val" -> v)).get
      coordinate(to, id)
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
