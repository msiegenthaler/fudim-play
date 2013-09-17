package models.dbcube

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import cube._

/** Cube of value type string. */
private case class DatabaseCubeString(repo: DatabaseCubeRepo, id: Long, table: String, dims: Map[Dimension, String], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends DatabaseCubeBase[String] {
  protected override type Self = DatabaseCubeString
  override def cubeType = DatabaseCubeString
  override def sqlType = "varchar(1024)"
  override def fromDb(name: String) = str(name)
  override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
  override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
}

private object DatabaseCubeString extends CubeType {
  override val tpeName = "string"
  override val tpeClass = classOf[String]
  override def apply(repo: DatabaseCubeRepo)(id: Long, table: String, dims: Map[Dimension, String]) =
    new DatabaseCubeString(repo, id, table, dims)
}
