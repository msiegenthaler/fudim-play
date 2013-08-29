package models.dbcube

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import cube._

/** Cube of value type int (64-bit signed). */
private case class DatabaseCubeLong(id: Long, table: String, dims: Map[Dimension, String], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends DatabaseCubeBase[Long] {
  protected override type Self = DatabaseCubeLong
  override def cubeType = DatabaseCubeLong
  override def sqlType = "bigint"
  override def fromDb(name: String) = long(name)
  override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
  override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
}

private object DatabaseCubeLong extends CubeType {
  override val tpeName = "long"
  override val tpeClass = classOf[Long]
  override def apply(id: Long, table: String, dims: Map[Dimension, String]) = new DatabaseCubeLong(id, table, dims)
}
