package models.dbcube

import anorm.SqlParser.str
import cube.Dimension
import support.DatabaseRepo
import java.sql.Connection

/** Cube of value type string. */
private object DatabaseCubeString extends CubeType {
  override val tpeName = "string"
  override val tpeClass = classOf[String]
  override def apply(cubeRepo: DatabaseCubeRepo)(identifier: Long, dbTable: String, dimensionMap: Map[Dimension, String]): DatabaseCube[String] =
    new DatabaseCubeBase[String] {
      override val id = identifier
      override val dims = dimensionMap
      override val table = dbTable
      override val repo = cubeRepo
      override def withConnection[A](f: Connection => A): A = cubeRepo.withConnection(f)
      override def cubeType = DatabaseCubeString
      override def sqlType = "varchar(1024)"
      override def fromDb(name: String) = str(name)
    }
}
