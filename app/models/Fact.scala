package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import java.sql.Connection
import models.cube._
import models.cube.db.DatabaseCubeData

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
  val cube: EditableCubeData[String]
  override def dimensions = cube.dimensions
  override def get(at: Point) = cube.get(at)
  override def set(at: Point, value: Option[String]) = cube.set(at, value)
  override def canSet(at: Point) = cube.isSettable(at)
}

private case class DataFact(name: String, cube: EditableCubeData[String]) extends DatabaseBackedFact

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    for {
      id ← SQL("select id from fact where name={name}").on("name" -> name).as(scalar[Long].singleOpt)
      cube ← DatabaseCubeData.load(name, classOf[String])
    } yield (DataFact(name, cube))
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    for {
      value ← SQL("select id, name from fact").as(long("id") ~ str("name") *)
      id ~ name = value
      cube ← DatabaseCubeData.load(name, classOf[String])
    } yield DataFact(name, cube)
  }

  def create(name: String, dims: Set[Dimension]): Fact = DB.withConnection { implicit c ⇒
    val cube = DatabaseCubeData.create(name, dims, classOf[String])
    val fact = DataFact(name, cube)
    SQL("insert into fact(name) values({name})").on("name" -> name).executeInsert().get
    fact
  }
}