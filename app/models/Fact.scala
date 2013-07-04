package models

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

  def get(at: Point): Option[String]
  /** Set the value at the point to value. Throws an ValueCannotBeSetException if not settable. */
  def set(at: Point, value: Option[String]): Unit = throw ValueCannotBeSetException(at)
  final def set(at: Point, value: String): Unit = set(at, Some(value))
  /** Whether the value at the point can be set. */
  def canSet(at: Point): Boolean = false
}

/** Fact that is backed by a database store for all fully defined points. */
sealed trait DatabaseBackedFact extends Fact {
  val cube: DatabaseCube[String]
  override def dimensions = cube.dimensions
  override def get(at: Point) = cube.get(at)
  override def set(at: Point, value: Option[String]) = cube.set(at, value)
  override def canSet(at: Point) = cube.isSettable(at)
}

private case class DatabaseFact(name: String, cube: DatabaseCube[String]) extends DatabaseBackedFact

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    for {
      id ~ name ~ cubeId ← SQL("select * from fact where name={name}").on("name" -> name).as(long("id") ~ str("name") ~ long("cube") singleOpt)
      cube ← DatabaseCube.load(cubeId, classOf[String])
    } yield (DatabaseFact(name, cube))
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    for {
      value ← SQL("select * from fact").as(long("id") ~ str("name") ~ long("cube") *)
      id ~ name ~ cubeId = value
      cube ← DatabaseCube.load(cubeId, classOf[String])
    } yield DatabaseFact(name, cube)
  }

  def create(name: String, dims: Set[Dimension]): Fact = DB.withConnection { implicit c ⇒
    val cube = DatabaseCube.create(dims, classOf[String])
    val fact = DatabaseFact(name, cube)
    SQL("insert into fact(name, cube) values({name}, {cube})").on("name" -> name, "cube" -> cube.id).executeInsert().get
    fact
  }
}