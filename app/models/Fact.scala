package models

import util.control.Exception._
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.libs.json._
import play.api.Play.current
import java.sql.Connection
import models.cube._
import models.cube.AggregateCube._
import models.cube.db.DatabaseCube

/** A fact has values for each coordinate in dimensions. */
sealed trait Fact {
  /** Unique name of the fact. */
  def name: String
  final def dimensions: Set[Dimension] = cube.dimensions

  def cube: Cube[String]
}

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    SQL("select * from fact where name={name}").on("name" -> name).as(fact singleOpt).flatten
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    SQL("select * from fact").as(fact *).flatten
  }

  private case class FactImpl(name: String, cube: Cube[String]) extends Fact
  private val fact = {
    long("id") ~ str("name") ~ str("config") map {
      case id ~ name ~ config ⇒
        val json = Json.parse(config)
        CubeFactory(json).map(cube ⇒ FactImpl(name, cube.asInstanceOf[Cube[String]]))
    }
  }

  def createDatabaseBacked(name: String, dims: Set[Dimension], aggregator: Option[Aggregator[String]]): Fact = DB.withConnection { implicit c ⇒
    val rawCube = DatabaseCube.create(dims, classOf[String])
    //    val cube = aggregator.map(CubeDecorator(rawCube, _)).getOrElse(rawCube)
    val cube = rawCube
    SQL("insert into fact(name, config) values({name}, {config})").on("name" -> name, "config" -> cubeConfig(cube)).executeInsert().get
    get(name).getOrElse(throw new IllegalStateException(s"creation of fact $name failed"))
  }

  def assignCube[C <: Cube[String]](fact: String, cube: C)(implicit jsonizable: JsonSerializable[C]): Fact = DB.withConnection { implicit c ⇒
    val updated = SQL("update fact set config={config} where name={name}").on("config" -> cubeConfig(cube), "name" -> fact).executeUpdate
    if (updated != 1) throw new IllegalArgumentException(s"no fact named $fact")
    FactImpl(fact, cube)
  }

  private def cubeConfig[C <: Cube[String]](cube: C)(implicit jsonizable: JsonSerializable[C]) = {
    val json = jsonizable.serialize(cube)
    Json.stringify(json)
  }

  object CubeFactory extends JsonCubeFactory {
    override val baseParsers = DatabaseCube.json :: Nil
  }
}