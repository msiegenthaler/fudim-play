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

/** A fact has values for each coordinate in dimensions. */
sealed trait Fact {
  /** Unique name of the fact. */
  def name: String

  def cube: Cube[String]
  protected def updateCube(aggregation: Aggregation): Unit

  def aggregation: Aggregation = cube match { case Aggregation(aggr) ⇒ aggr }
  def aggregation_=(aggr: Aggregation) = updateCube(aggr)

  def dimensions: Set[Dimension] = cube.dimensions
  def addDimension(moveTo: Coordinate): Unit
  def removeDimension(keepAt: Coordinate): Unit
}

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    SQL("select * from fact where name={name}").on("name" -> name).as(fact singleOpt).flatten
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    SQL("select * from fact").as(fact *).flatten
  }

  private class DatabaseFact(val name: String, private var _cube: Cube[String]) extends Fact {
    override def cube = _cube
    def databaseCube = CubeDecorator.undecorateComplete(cube) match {
      case d: DatabaseCube[String] ⇒ d
      case _ ⇒ throw new IllegalStateException("our cube is not a database cube")
    }
    protected def updateCube(databaseCube: DatabaseCube[String], aggr: Aggregation) = {
      val newCube = aggr.aggregator.map(a ⇒ CubeDecorator(databaseCube, a)).
        getOrElse(databaseCube)
      assignCube(name, newCube)
      _cube = newCube
    }
    override protected def updateCube(aggr: Aggregation) = updateCube(databaseCube, aggr)
    def addDimension(moveTo: Coordinate) =
      updateCube(databaseCube.copyAndAddDimension(moveTo), aggregation)
    def removeDimension(keepAt: Coordinate) =
      updateCube(databaseCube.copyAndRemoveDimension(keepAt), aggregation)
  }
  private val fact = {
    long("id") ~ str("name") ~ str("config") map {
      case id ~ name ~ config ⇒
        val json = Json.parse(config)
        val cube = JsonMappers.cube.parse(json).map(cube ⇒ new DatabaseFact(name, cube.asInstanceOf[Cube[String]]))
        cube.leftMap(msg ⇒ Logger.info(s"Could not load cube for fact $name: $msg"))
        cube.toOption
    }
  }

  def createDatabaseBacked(name: String, dims: Set[Dimension], aggregator: Option[Aggregator[String]]): Fact = DB.withConnection { implicit c ⇒
    val rawCube = DatabaseCube.create(dims, classOf[String])
    val cube = aggregator.map(CubeDecorator(rawCube, _)).getOrElse(rawCube)
    SQL("insert into fact(name, config) values({name}, {config})").on("name" -> name, "config" -> cubeConfig(cube)).executeInsert().get
    get(name).getOrElse(throw new IllegalStateException(s"creation of fact $name failed, see log"))
  }

  private def assignCube[C <: Cube[String]](fact: String, cube: C): Unit = DB.withConnection { implicit c ⇒
    val updated = SQL("update fact set config={config} where name={name}").on("config" -> cubeConfig(cube), "name" -> fact).executeUpdate
    if (updated != 1) throw new IllegalArgumentException(s"no fact named $fact")
  }

  private def cubeConfig[C <: Cube[String]](cube: C) = {
    val json = JsonMappers.cube.serialize(cube).
      leftMap(msg ⇒ Logger.info(s"Could not serialize cube $cube: $msg")).
      getOrElse(throw new IllegalArgumentException(s"Cube $cube is not serializable"))
    Json.stringify(json)
  }
}