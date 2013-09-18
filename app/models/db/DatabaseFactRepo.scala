package models.db

import util.control.Exception._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import play.api.libs.json._
import cube._
import domain._
import models._
import models.dbcube._

trait DatabaseFactRepo extends FudimFactRepo with DatabaseRepo {
  protected def jsonCubeMapperRepo: JsonCubeMapperRepository
  protected def databaseCubeRepo: DatabaseCubeRepo
  def domain: DomainId

  override def get[T](name: String) = withConnection { implicit c ⇒
    SQL("select * from fact where domain={domain} and name={name}").on("domain" -> domain.id, "name" -> name).as(fact singleOpt).flatten
  }

  override def all = withConnection { implicit c ⇒
    SQL("select * from fact where domain={domain}").on("domain" -> domain.id).as(fact *).flatten
  }

  override def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dims: Set[Dimension], aggregator: Option[Aggregator[T]]) = withConnection { implicit c ⇒
    val rawCube = databaseCubeRepo.create(dims, dataType.tpe)
    val cube: Cube[T] = aggregator.map(CubeDecorator(rawCube, _)).getOrElse(rawCube)
    SQL("insert into fact(domain, name, type, config) values({domain}, {name}, {type}, {config})").
      on("domain" -> domain.id, "name" -> name, "config" -> cubeConfig(cube), "type" -> dataType.name).
      executeInsert().get
    get(name).
      map(_.asInstanceOf[FudimFact[T]]).
      getOrElse(throw new IllegalStateException(s"Creation of fact $name failed, see log"))
  }

  override def remove(name: String) = {
    get(name).foreach {
      case fact: DatabaseFact[_] ⇒
        databaseCubeRepo.delete(fact.databaseCube)
        SQL("delete from fact where id={id}").on("id" -> fact.id)
    }
  }

  private class DatabaseFact[T](val id: Long, val name: String, val dataType: FudimDataType[T], private var _cube: Cube[T]) extends FudimFact[T] {
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
    def mkFact(id: Long, name: String, dataType: FudimDataType[_], cube: Cube[_]): FudimFact[dataType.Type] = {
      type T = dataType.Type
      (dataType, cube) match {
        case (dataType: DataType[T], cube: Cube[T]) => new DatabaseFact[T](id, name, dataType, cube)
        case _ => throw new IllegalStateException("Non matching dataType and cube")
      }
    }

    import scalaz._
    import Scalaz._
    long("id") ~ str("name") ~ str("type") ~ str("config") map {
      case id ~ name ~ typeName ~ config ⇒
        val fact: Validation[String, FudimFact[_]] = for {
          dataType ← FudimDataTypes.get(typeName).toSuccess(s"DataType $typeName is not known")
          json = Json.parse(config)
          cube ← jsonCubeMapperRepo.parse(json)
        } yield mkFact(id, name, dataType, cube)
        fact.leftMap(msg ⇒ Logger.info(s"Could not load fact $name: $msg"))
        fact.toOption
    }
  }

  private def assignCube[T](fact: String, cube: Cube[T]): Unit = withConnection { implicit c ⇒
    val updated = SQL("update fact set config={config} where name={name}").on("config" -> cubeConfig(cube), "name" -> fact).executeUpdate
    if (updated != 1) throw new IllegalArgumentException(s"no fact named $fact")
  }

  private def cubeConfig(cube: Cube[_]) = {
    val json = jsonCubeMapperRepo.serialize(cube).
      leftMap(msg ⇒ Logger.info(s"Could not serialize cube $cube: $msg")).
      getOrElse(throw new IllegalArgumentException(s"Cube $cube is not serializable"))
    Json.stringify(json)
  }
}
