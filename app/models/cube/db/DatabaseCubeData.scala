package models.cube.db

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models._
import models.cube._
import CubeData._
import java.sql.Connection

object DatabaseCubeData {
  private case class CubeDefinition(id: Long, name: String, tpe: String) {
    def tableName = s"databaseCube_data_$id"
    def dimensionName(d: Dimension) = "dim_" + Dimension.idOf(d)
  }
  private val cubeDefinition = {
    get[Long]("id") ~
      get[String]("name") ~ get[String]("type") map {
        case id ~ name ~ tpe ⇒ CubeDefinition(id, name, tpe)
      }
  }

  def load[T](name: String, tpe: Class[T]): Option[EditableCubeData[T]] = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where name={name}").on("name" -> name).as(cubeDefinition.singleOpt).map { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      val cubeTypeExpect = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
      if (cubeType != cubeTypeExpect)
        throw new IllegalArgumentException(s"type of cube $name does not match: expected $cubeType but is $cubeTypeExpect")
      cube.asInstanceOf[DCDBase[T]]
    }
  }

  def create[T](name: String, dims: Set[Dimension], tpe: Class[T]): EditableCubeData[T] = DB.withConnection { implicit c ⇒
    if (SQL("select count(*) from databaseCube where name={name}").on("name" -> name).as(scalar[Long].single) > 0)
      throw new IllegalArgumentException(s"Cube $name already exists")

    val cubeType = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
    val id = SQL("insert into databaseCube(name, type) values({name}, {type})").on("name" -> name, "type" -> cubeType.tpeName).executeInsert().
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database ($name)"))
    val definition = CubeDefinition(id, name, cubeType.tpeName)

    val cdims = dims.map { dim ⇒
      SQL("insert into databaseCube_dimension(cube, dimension) values ({cube}, {dimension})").
        on("cube" -> id, "dimension" -> Dimension.idOf(dim)).executeInsert()
      (dim, definition.dimensionName(dim))
    }.toMap

    val cube = cubeType(definition.tableName, cdims)
    cube.create
    cube.asInstanceOf[DCDBase[T]]
  }

  def delete(name: String) = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where name={name}").on("name" -> name).as(cubeDefinition.singleOpt).foreach { definition ⇒
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
    val cube = cubeType(definition.tableName, dims)
    (cube, cubeType)
  }

  private val typeMapping: Map[Class[_], CubeType] = {
    val list = DCDString :: Nil
    list.map(e ⇒ (e.tpeClass, e)).toMap
  }
}