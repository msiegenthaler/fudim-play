package models.dbcube

import java.sql.Connection
import anorm.SqlParser.int
import cube.Dimension
import support.DatabaseRepo

/** Cube of value type int (32-bit signed). */
private object DatabaseCubeInt extends CubeType {
  override val tpeName = "integer"
  override val tpeClass = classOf[Int]
  override def apply(cubeRepo: DatabaseCubeRepo)(identifier: Long, dbTable: String, dimensionMap: Map[Dimension, String]): DatabaseCube[Int] =
    new DatabaseCubeBase[Int] {
      override val id = identifier
      override val dims = dimensionMap
      override val table = dbTable
      override val repo = cubeRepo
      override def withConnection[A](f: Connection => A): A = cubeRepo.withConnection(f)
      override def cubeType = DatabaseCubeInt
      override def sqlType = "int"
      override def fromDb(name: String) = int(name)
    }
}
