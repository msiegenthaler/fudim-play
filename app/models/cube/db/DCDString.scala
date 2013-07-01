package models.cube.db

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import models.cube._
import CubeData._

/** Cube of value type string. */
private case class DCDString(id: Long, table: String, dims: Map[Dimension, String], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends DCDBase[String] {
  override def sqlType = "varchar(1024)"
  override def fromDb(name: String) = str(name)
  override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
  override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
}

private object DCDString extends CubeType {
  override val tpeName = "string"
  override val tpeClass = classOf[String]
  override def apply(id: Long, table: String, dims: Map[Dimension, String]) = new DCDString(id, table, dims)
}
