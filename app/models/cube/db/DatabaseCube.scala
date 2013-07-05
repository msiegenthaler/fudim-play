package models.cube.db

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import models.cube._
import Cube._
import java.sql.Connection

trait DatabaseCube[T] extends EditableCube[T] {
  protected override type Self <: DatabaseCube[T]
  def id: Long

  private[db] def create: Unit
  private[db] def drop: Unit

  /**
   * Creates a copy of this cube with identical data but an additional dimension.
   * Existing data will be assigned the specified coordinate in the new dimension.
   */
  def copyAndAddDimension(moveTo: Coordinate): Self = ???
  /**
   * Creates a copy of this cube with identical data but a removed dimension.
   *  Only the data at specified coordinate will be kept.
   */
  def copyAndRemoveDimension(keepAt: Coordinate): Self = ???
}
object DatabaseCube {
  private case class CubeDefinition(id: Long, tpe: String) {
    def tableName = s"databaseCube_data_$id"
    def dimensionName(d: Dimension) = "dim_" + Dimension.idOf(d)
  }
  private val cubeDefinition = {
    get[Long]("id") ~ get[String]("type") map {
      case id ~ tpe ⇒ CubeDefinition(id, tpe)
    }
  }

  def load[T](id: Long, tpe: Class[T]): Option[DatabaseCube[T]] = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> id).as(cubeDefinition.singleOpt).map { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      val cubeTypeExpect = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
      if (cubeType != cubeTypeExpect)
        throw new IllegalArgumentException(s"type of cube $id does not match: expected $cubeType but is $cubeTypeExpect")
      cube.asInstanceOf[DatabaseCube[T]]
    }
  }

  def create[T](dims: Set[Dimension], tpe: Class[T]): DatabaseCube[T] = DB.withConnection { implicit c ⇒
    val cubeType = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
    val id = SQL("insert into databaseCube(type) values({type})").on("type" -> cubeType.tpeName).executeInsert().
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database"))
    val definition = CubeDefinition(id, cubeType.tpeName)

    val cdims = dims.map { dim ⇒
      SQL("insert into databaseCube_dimension(cube, dimension) values ({cube}, {dimension})").
        on("cube" -> id, "dimension" -> Dimension.idOf(dim)).executeInsert()
      (dim, definition.dimensionName(dim))
    }.toMap

    val cube = cubeType(id, definition.tableName, cdims)
    cube.create
    cube.asInstanceOf[DatabaseCube[T]]
  }

  def delete(cube: DatabaseCube[_]) = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> cube.id).as(cubeDefinition.singleOpt).foreach { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      cube.drop
      SQL("delete from databaseCube_dimension where cube={id}").on("id" -> definition.id).executeUpdate
      SQL("delete from databaseCube where id={id}").on("id" -> definition.id).executeUpdate
    }
  }

  private def loadFromDefinition(definition: CubeDefinition)(implicit c: Connection) = {
    val dims = SQL("select d.id as id, d.name as name from databaseCube_dimension dcd inner join dimension d on d.id = dcd.dimension where dcd.cube={id}").on("id" -> definition.id).
      as(get[Long]("id") ~ get[String]("name") *).map {
        case id ~ name ⇒ (Dimension.get(name).get, s"dim_$id")
      }.toMap
    val cubeType = typeMapping.values.find(_.tpeName == definition.tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube db-type: ${definition.tpe}"))
    val cube = cubeType(definition.id, definition.tableName, dims)
    (cube, cubeType)
  }

  private val typeMapping: Map[Class[_], CubeType] = {
    val list = DatabaseCubeString :: Nil
    list.map(e ⇒ (e.tpeClass, e)).toMap
  }
}

private trait CubeType {
  val tpeName: String
  val tpeClass: Class[_]
  def apply(id: Long, table: String, dims: Map[Dimension, String]): DatabaseCube[_]
  override def toString = tpeName
}