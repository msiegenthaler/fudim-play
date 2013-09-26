package models.dbcube

import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import cube._
import support.DatabaseRepo
import java.sql.Connection

trait DatabaseCube[T] {
  def id: Long

  def cube: Cube[T]
  def editor: CubeEditor[T]

  protected[dbcube] def cubeType: CubeType
  protected[dbcube] def table: String
  protected[dbcube] def dims: Map[Dimension, String]
  protected[dbcube] def create: Unit
  protected[dbcube] def drop: Unit


  /**
   * Creates a copy of this cube with identical data but an additional dimension.
   * Existing data will be assigned the specified coordinate in the new dimension.
   */
  def copyAndAddDimension(moveTo: Coordinate): DatabaseCube[T]
  /**
   * Creates a copy of this cube with identical data but a removed dimension.
   * Only the data at specified coordinate will be kept.
   */
  def copyAndRemoveDimension(keepAt: Coordinate): DatabaseCube[T]
}

trait DatabaseCubeRepo extends DatabaseRepo {
  protected def dimension(name: String): Option[Dimension]

  private case class CubeDefinition(id: Long, tpe: String) {
    def tableName = s"databaseCube_data_$id"
  }
  private val cubeDefinition = {
    get[Long]("id") ~ get[String]("type") map {
      case id ~ tpe ⇒ CubeDefinition(id, tpe)
    }
  }

  def load[T](id: Long, tpe: Class[T]): Option[DatabaseCube[T]] = withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> id).as(cubeDefinition.singleOpt).map { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      val cubeTypeExpect = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
      if (cubeType != cubeTypeExpect)
        throw new IllegalArgumentException(s"type of cube $id does not match: expected $cubeType but is $cubeTypeExpect")
      cube.asInstanceOf[DatabaseCube[T]]
    }
  }
  def load[T](id: Long): Option[DatabaseCube[_]] = withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> id).as(cubeDefinition.singleOpt).
      map(loadFromDefinition).map(_._1)
  }

  def create[T](dims: Set[Dimension], tpe: Class[T]): DatabaseCube[T] = withConnection { implicit c ⇒
    val cubeType = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
    val id = SQL("insert into databaseCube(type) values({type})").on("type" -> cubeType.tpeName).executeInsert().
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database"))
    val definition = CubeDefinition(id, cubeType.tpeName)

    val cdims = dims.map { dim ⇒
      val dimId = SQL("insert into databaseCube_dimension(cube, dimension) values ({cube}, {dimension})").
        on("cube" -> id, "dimension" -> dim.name).executeInsert().map("dim_" + _).get
      (dim, dimId)
    }.toMap

    val cube = cubeType(this)(id, definition.tableName, cdims)
    cube.create
    cube.asInstanceOf[DatabaseCube[T]]
  }

  def delete(cube: DatabaseCube[_]) = withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> cube.id).as(cubeDefinition.singleOpt).foreach { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      cube.drop
      SQL("delete from databaseCube_dimension where cube={id}").on("id" -> definition.id).executeUpdate
      SQL("delete from databaseCube where id={id}").on("id" -> definition.id).executeUpdate
    }
  }

  private def loadFromDefinition(definition: CubeDefinition)(implicit c: Connection) = {
    val dims = SQL("select id, dimension from databaseCube_dimension where cube={id}").on("id" -> definition.id).
      as(get[Long]("id") ~ get[String]("dimension") *).map {
      case id ~ name ⇒
        val d: Dimension = dimension(name).getOrElse(throw new IllegalStateException(s"Could not find dimension $name"))
        (d, s"dim_$id")
    }.toMap
    val cubeType = typeMapping.values.find(_.tpeName == definition.tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube db-type: ${definition.tpe}"))
    val cube = cubeType(this)(definition.id, definition.tableName, dims)
    (cube, cubeType)
  }
  override protected[dbcube] def withConnection[A](f: (Connection) => A): A

  private val typeMapping: Map[Class[_], CubeType] = {
    val list = DatabaseCubeString :: DatabaseCubeInt :: DatabaseCubeLong :: Nil
    list.map(e ⇒ (e.tpeClass, e)).toMap
  }

  def json = new JsonCubeMapper {
    import scalaz._
    import Scalaz._
    override val id = "databaseCube"
    override def parser = json ⇒
      for {
        id ← (json \ "id").asOpt[Long].toSuccess("Missing value 'id'")
        cube ← load(id).toSuccess(s"Could not find cube with id $id in database")
      } yield cube.cube
    override def serializer = {
      case cube: DatabaseCube[_] ⇒ Json.obj("id" -> cube.id).success
    }
  }
}

private trait CubeType {
  val tpeName: String
  val tpeClass: Class[_]
  def apply(repo: DatabaseCubeRepo)(id: Long, table: String, dims: Map[Dimension, String]): DatabaseCube[_]
  override def toString = tpeName
}
