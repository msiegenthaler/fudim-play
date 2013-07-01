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
  override def get(at: Point) = cube.get(at)
  override def set(at: Point, value: Option[String]) = cube.set(at, value)
  override def canSet(at: Point) = cube.isSettable(at)
}

private case class DataFact(name: String, dimensions: Set[Dimension], cube: EditableCubeData[String]) extends DatabaseBackedFact

object Fact {
  def get(name: String): Option[Fact] = DB.withConnection { implicit c ⇒
    for {
      id ← SQL("select id from fact where name={name}").on("name" -> name).as(scalar[Long].singleOpt)
      cube ← DatabaseCubeData.load(name, classOf[String])
      dims = dimensionsFor(id)
    } yield (DataFact(name, dims, cube))
  }
  def all: Iterable[Fact] = DB.withConnection { implicit c ⇒
    for {
      value ← SQL("select id, name from fact").as(long("id") ~ str("name") *)
      id ~ name = value
      dims = dimensionsFor(id)
      cube ← DatabaseCubeData.load(name, classOf[String])
    } yield DataFact(name, dims, cube)
  }

  private def dimensionsFor(factId: Long)(implicit c: Connection) = {
    SQL("""select d.* from fact_dimension fd
           left outer join dimension d on d.id = fd.dimension
           where fd.fact = {id}""").on("id" -> factId).as(Dimension.dimension *).toSet
  }

  def create(name: String, dims: Set[Dimension]): Fact = DB.withConnection { implicit c ⇒
    val cube = DatabaseCubeData.create(name, dims, classOf[String])
    val fact = DataFact(name, dims, cube)
    save(fact)
    fact
  }
  def save(fact: Fact) = DB.withConnection { implicit c ⇒
    val id = SQL("select id from fact where name={name}").on("name" -> fact.name).as(scalar[Long].singleOpt) match {
      case Some(id) ⇒
        SQL("update fact set name={name} where id={id}").on("name" -> fact.name, "id" -> id).executeUpdate
        SQL("delete from fact_dimension where fact={fact}").on("fact" -> id).executeUpdate
        id
      case None ⇒
        SQL("insert into fact(name) values({name})").on("name" -> fact.name).executeInsert().get
    }
    fact.dimensions.foreach { d ⇒
      SQL("insert into fact_dimension(fact, dimension) values({fact},{dim})").on("fact" -> id, "dim" -> Dimension.idOf(d)).executeUpdate
    }
  }
}