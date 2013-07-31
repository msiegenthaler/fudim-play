package models

import util.control.Exception._
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.libs.json._
import play.api.Play.current
import java.sql.Connection
import models.cube._
import models.cube.db.DatabaseCube
import models.cube.JsonCube

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

  def createDatabaseBacked(name: String, dims: Set[Dimension]): Fact = DB.withConnection { implicit c ⇒
    val cube = DatabaseCube.create(dims, classOf[String])
    SQL("insert into fact(name, config) values({name}, {config})").on("name" -> name, "config" -> cubeConfig(cube)).executeInsert().get
    FactImpl(name, cube)
  }

  def assignCube(fact: String, cube: JsonCube[String]): Fact = DB.withConnection { implicit c ⇒
    val updated = SQL("update fact set config={config} where name={name}").on("config" -> cubeConfig(cube), "name" -> fact).executeUpdate
    if (updated != 1) throw new IllegalArgumentException(s"no fact named $fact")
    FactImpl(fact, cube)
  }

  private def cubeConfig(c: JsonCube[_]) = Json.stringify(c.asJson)

  //TODO replace with something efficient, this is just for a demo
  private def aggregator = Aggregators.fold(Some("0"))(sumIfNumber)
  private def sumIfNumber(oa: Option[String], b: String): Option[String] = {
    for {
      a ← oa
      na ← catching(classOf[NumberFormatException]).opt(a.toLong)
      nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
    } yield (na + nb).toString
  }

  object CubeFactory extends JsonCubeFactory {
    override val baseParsers = DatabaseCube :: Nil
  }
}