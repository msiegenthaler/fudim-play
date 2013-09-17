package models.dbcube

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import cube._

/** Cube of value type int (32-bit signed). */
private case class DatabaseCubeInt(repo: DatabaseCubeRepo, id: Long, table: String, dims: Map[Dimension, String], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends DatabaseCubeBase[Int] {
  protected override type Self = DatabaseCubeInt
  override def cubeType = DatabaseCubeInt
  override def sqlType = "integer"
  override def fromDb(name: String) = int(name)
  override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
  override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
}

private object DatabaseCubeInt extends CubeType {
  override val tpeName = "int"
  override val tpeClass = classOf[Int]
  override def apply(repo: DatabaseCubeRepo)(id: Long, table: String, dims: Map[Dimension, String]) =
    new DatabaseCubeInt(repo, id, table, dims)
}
