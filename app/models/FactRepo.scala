package models

import util.control.Exception._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import play.api.db._
import play.api.libs.json._
import play.api.Play.current
import java.sql.Connection
import cube._
import models.dbcube.DatabaseCube

object FactRepo {
  def get[T](name: String): Option[FudimFact[Any]] = DB.withConnection { implicit c ⇒
    SQL("select * from fact where name={name}").on("name" -> name).as(fact singleOpt).flatten
  }
  def all: Iterable[FudimFact[_]] = DB.withConnection { implicit c ⇒
    SQL("select * from fact").as(fact *).flatten
  }

  private class DatabaseFact[T](val name: String, val dataType: DataType[T], private var _cube: Cube[T]) extends FudimFact[T] {
    override def data = _cube
    def databaseCube = CubeDecorator.undecorateComplete(data) match {
      case d: DatabaseCube[T] ⇒ d
      case _ ⇒ throw new IllegalStateException("our cube is not a database cube")
    }
    protected def updateCube(databaseCube: DatabaseCube[T], aggr: Aggregation[T]) = {
      val newCube = aggr.aggregator.map(a ⇒ CubeDecorator(databaseCube, a)).
        getOrElse(databaseCube)
      assignCube(name, newCube)
      _cube = newCube
    }
    override protected def updateCube(aggr: Aggregation[T]) = updateCube(databaseCube, aggr)
    def addDimension(moveTo: Coordinate) =
      updateCube(databaseCube.copyAndAddDimension(moveTo), aggregation)
    def removeDimension(keepAt: Coordinate) =
      updateCube(databaseCube.copyAndRemoveDimension(keepAt), aggregation)
  }
  private val fact = {
    import scalaz._
    import Scalaz._
    long("id") ~ str("name") ~ str("type") ~ str("config") map {
      case id ~ name ~ typeName ~ config ⇒
        val fact = for {
          dataType ← DataType.get(typeName).toSuccess(s"DataType $typeName is not known")
          json = Json.parse(config)
          cube ← JsonMappers.cube.parse(json)
        } yield new DatabaseFact(name, dataType.asInstanceOf[DataType[Any]], cube.asInstanceOf[Cube[Any]])
        fact.leftMap(msg ⇒ Logger.info(s"Could not load fact $name: $msg"))
        fact.toOption
    }
  }

  def createDatabaseBacked[T](name: String, dataType: DataType[T], dims: Set[Dimension], aggregator: Option[Aggregator[T]]): FudimFact[T] = DB.withConnection { implicit c ⇒
    val rawCube = DatabaseCube.create(dims, dataType.tpe)
    val cube: Cube[T] = aggregator.map(CubeDecorator(rawCube, _)).getOrElse(rawCube)
    SQL("insert into fact(name, type, config) values({name}, {type}, {config})").
      on("name" -> name, "config" -> cubeConfig(cube), "type" -> dataType.name).
      executeInsert().get
    get(name).
      map(_.asInstanceOf[FudimFact[T]]).
      getOrElse(throw new IllegalStateException(s"Creation of fact $name failed, see log"))
  }

  private def assignCube[T](fact: String, cube: Cube[T]): Unit = DB.withConnection { implicit c ⇒
    val updated = SQL("update fact set config={config} where name={name}").on("config" -> cubeConfig(cube), "name" -> fact).executeUpdate
    if (updated != 1) throw new IllegalArgumentException(s"no fact named $fact")
  }

  private def cubeConfig(cube: Cube[_]) = {
    val json = JsonMappers.cube.serialize(cube).
      leftMap(msg ⇒ Logger.info(s"Could not serialize cube $cube: $msg")).
      getOrElse(throw new IllegalArgumentException(s"Cube $cube is not serializable"))
    Json.stringify(json)
  }
}