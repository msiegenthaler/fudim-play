package models

import util.control.Exception._
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.Connection
import models.cube._
import models.cube.db.DatabaseCube

/** A fact has values for each coordinate in dimensions. */
sealed trait Fact {
  /** Unique name of the fact. */
  def name: String
  /** Dimensions that fully define the fact. */
  def dimensions: Set[Dimension]

  def cube: EditableCube[String]

  /** Add a dimension and assign all existing values to the defined coordinate. The dimension to add is defined by the coordinate. */
  def addDimension(moveTo: Coordinate): Fact
  /** Remove a dimension, only the values at the given coordinate are preserved. */
  def removeDimension(keepAt: Coordinate): Fact
}

/** Fact that is backed by a database store for all fully defined points. */
sealed trait DatabaseBackedFact extends Fact {
  override def dimensions = cube.dimensions
}

private case class DatabaseFact(name: String, dbCube: DatabaseCube[String], aggr: Aggregator[String]) extends DatabaseBackedFact {
  override val cube: EditableCube[String] = AggregateCube(dbCube, aggr)
  override def addDimension(moveTo: Coordinate) = {
    changeCube(dbCube.copyAndAddDimension(moveTo))
  }
  override def removeDimension(keepAt: Coordinate) = {
    changeCube(dbCube.copyAndRemoveDimension(keepAt))
  }
  private def changeCube(newCube: DatabaseCube[String]) = {
    Fact.assignCube(name, newCube)
    DatabaseCube.delete(dbCube)
    copy(dbCube = newCube)
  }
}

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    for {
      id ~ name ~ cubeId ← SQL("select * from fact where name={name}").on("name" -> name).as(long("id") ~ str("name") ~ long("cube") singleOpt)
      cube ← DatabaseCube.load(cubeId, classOf[String])
    } yield (DatabaseFact(name, cube, aggregator))
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    for {
      value ← SQL("select * from fact").as(long("id") ~ str("name") ~ long("cube") *)
      id ~ name ~ cubeId = value
      cube ← DatabaseCube.load(cubeId, classOf[String])
    } yield DatabaseFact(name, cube, aggregator)
  }

  def create(name: String, dims: Set[Dimension]): Fact = DB.withConnection { implicit c ⇒
    val cube = DatabaseCube.create(dims, classOf[String])
    val fact = DatabaseFact(name, cube, aggregator)
    SQL("insert into fact(name, cube) values({name}, {cube})").on("name" -> name, "cube" -> cube.id).executeInsert().get
    fact
  }

  private[models] def assignCube(name: String, cube: DatabaseCube[_]) = DB.withConnection { implicit c ⇒
    SQL("update fact set cube={cube} where name={name}").on("cube" -> cube.id, "name" -> name).executeUpdate == 1
  }

  //TODO replace with something efficient, this is just for a demo
  private def aggregator = Aggregators.fold(Some("0"))(sumIfNumber)
  private def sumIfNumber(oa: Option[String], b: String): Option[String] = {
    for {
      a ← oa
      na ← catching(classOf[NumberFormatException]).opt(a.toLong)
      nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
    } yield (na + nb).toString
  }
}