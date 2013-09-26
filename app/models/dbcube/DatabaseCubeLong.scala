package models.dbcube

import java.sql.Connection
import anorm.SqlParser.long
import cube.Dimension
import support.DatabaseRepo

/** Cube of value type int (64-bit signed). */
private object DatabaseCubeLong extends CubeType {
  override val tpeName = "long"
  override val tpeClass = classOf[Long]
  override def apply(cubeRepo: DatabaseCubeRepo)(identifier: Long, dbTable: String, dimensionMap: Map[Dimension, String]): DatabaseCube[Long] =
    new DatabaseCubeBase[Long] {
      override val id = identifier
      override val dims = dimensionMap
      override val table = dbTable
      override val repo = cubeRepo
      override def withConnection[A](f: Connection => A): A = cubeRepo.withConnection(f)
      override def cubeType = DatabaseCubeLong
      override def sqlType = "bigint"
      override def fromDb(name: String) = long(name)
    }
}
