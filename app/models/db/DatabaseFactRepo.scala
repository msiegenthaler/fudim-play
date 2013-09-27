package models
package db

import scalaz._
import Scalaz._
import anorm._
import anorm.SqlParser._
import cube._
import domain._
import support.DatabaseRepo
import play.api.Logger
import play.api.libs.json._

trait DatabaseFactRepo extends FudimFactRepo with DatabaseRepo {
  protected def domain: FudimDomain
  protected def dataTypeRepo = FudimDataTypes
  protected def jsonFormulaRepo: JsonFormulaMapperRepository
  protected def cubeDataStoreRepo: CopyableCubeDataStoreRepo

  override def get(name: String) = withConnection { implicit c ⇒
    SQL("select * from fact where domain={domain} and name={name}").on("domain" -> domain.id.id, "name" -> name).as(fact singleOpt).flatten
  }

  override def all = withConnection { implicit c ⇒
    SQL("select * from fact where domain={domain}").on("domain" -> domain.id.id).as(fact *).flatten
  }

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]) = {
    val backend = DataStoreFactBackend(dataType, dimensions, aggregation)
    create(name, backend)
  }
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]) = {
    val backend = FormulaFactBackend(dataType, formula, aggregation)
    create(name, backend)
  }
  protected def create[T](name: String, backend: FactBackend[T]): DatabaseFact[T] = withConnection { implicit c =>
    val config = Json.stringify(backend.config)
    val id = SQL("insert into fact(domain, name, dataType, factType, config) values({domain}, {name}, {dataType}, {factType}, {config})").
      on("domain" -> domain.id.id, "name" -> name, "dataType" -> backend.dataType.name, "factType" -> backend.factType, "config" -> config).
      executeInsert().get
    new DatabaseFact(id, name, backend)
  }

  def remove(name: String) = {
    get(name).foreach {
      case fact: DatabaseFact[_] =>
        fact.delete
        SQL("delete from fact where id={id}").on("id" -> fact.id)
    }
  }

  private val fact = {
    long("id") ~ str("name") ~ str("dataType") ~ str("factType") ~ str("config") map {
      case id ~ name ~ dataTypeName ~ factType ~ config ⇒
        val fact: Validation[String, FudimFact[_]] = for {
          dataType ← dataTypeRepo.get(dataTypeName).toSuccess(s"DataType $dataTypeName is not known")
          json = Json.parse(config)
          backend <- {
            val mk: PartialFunction[String, FactBackend[_]] = {
              case DataStoreFactBackend.key => DataStoreFactBackend(dataType, json)
            }
            mk.lift(factType).toSuccess("Unknown Fact type $factType")
          }
        } yield new DatabaseFact(id, name, backend)
        fact.leftMap(msg ⇒ Logger.warn(s"Could not load fact $name: $msg"))
        fact.toOption
    }
  }

  protected final class DatabaseFact[T](val id: Long, val name: String, ib: FactBackend[T]) extends FudimFact[T] {
    private[this] var _backend: FactBackend[T] = ib
    def backend = synchronized(_backend)
    def backend_=(nb: FactBackend[T]) = withConnection { implicit c =>
      val config = Json.stringify(backend.config)
      val updated = SQL("update fact set config={config} where id={id}").on("config" -> config, "id" -> id).executeUpdate
      if (updated != 1) throw new IllegalStateException(s"Fact with id $id (named $name) was not found anymore.")
    }


    override def dataType = backend.dataType
    def data = backend.data
    def editor = backend.editor
    def aggregation = backend.aggregation
    def aggregation_=(aggr: Aggregation[T]) = backend = backend.aggregation(aggregation = aggr)
    def addDimension(moveTo: Coordinate) = backend = backend.addDimension(moveTo)
    def removeDimension(keepAt: Coordinate) = backend = backend.removeDimension(keepAt)
    def delete() = backend.delete()

    override def equals(o: Any) = {
      if (o.isInstanceOf[DatabaseFact[_]]) o.asInstanceOf[DatabaseFact[_]].id == id
      else false
    }
    override def hashCode = id.hashCode
    override def toString = s"DatabaseFact(id=$id, name=$name, factType=${backend.factType}, dataType=$dataType)"
  }

  protected trait FactBackend[T] {
    def factType: String
    def config: JsValue

    def dataType: FudimDataType[T]
    def data: Cube[T]
    def editor: Option[CubeEditor[T]]
    def aggregation: Aggregation[T]

    def aggregation(aggregation: Aggregation[T] = aggregation): FactBackend[T]
    def addDimension(moveTo: Coordinate): FactBackend[T]
    def removeDimension(keepAt: Coordinate): FactBackend[T]
    def delete(): Unit = ()
  }

  private def aggregationFromJson(json: JsValue) = for {
    aggregationName <- json.asOpt[String].toSuccess("Missing aggregation in config")
    aggregation <- Aggregation.all.find(_.name == aggregationName).toSuccess(s"Aggregation $aggregationName does not exist")
  } yield aggregation

  private case class DataStoreFactBackend[T](dataType: FudimDataType[T], cds: CopyableCubeDataStore[T], aggregation: Aggregation[T]) extends FactBackend[T] {
    override def factType = DataStoreFactBackend.key
    override val data = aggregation.aggregator match {
      case Some(aggr) => CubeDecorator(cds.cube, Aggregator(aggr))
      case None => cds.cube
    }
    override val editor = Some(cds.editor)

    override def aggregation(aggregation: Aggregation[T]) = copy(aggregation = aggregation)
    override def addDimension(moveTo: Coordinate) = {
      val newCds = cds.copy(moveTo, Point.empty)
      delete()
      copy(cds = newCds)
    }
    override def removeDimension(keepAt: Coordinate) = {
      val newCds = cds.copy(Point.empty, keepAt)
      delete()
      copy(cds = newCds)
    }
    override def delete() = cubeDataStoreRepo.remove(cds.id)

    override def config = {
      Json.obj(
        "cubeDataStore-id" -> cds.id,
        "aggregation" -> aggregation.name
      )
    }
  }
  private object DataStoreFactBackend {
    val key = "dataStore"
    def apply[T](dataType: FudimDataType[T], config: JsValue) = {
      val backend = for {
        id <- (config \ "cubeDataStore-id").asOpt[Long].toSuccess("Missing cubeDataStore-id")
        cds <- cubeDataStoreRepo.get(id, dataType).toSuccess("CubeDataStore not found")
        aggregation <- aggregationFromJson(config \ "aggregation")
      } yield new DataStoreFactBackend(dataType, cds, aggregation.asInstanceOf[Aggregation[T]])
      backend.valueOr(e => throw new IllegalStateException(s"Cannot load cds-fact from config: $e"))
    }
    def apply[T](dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]) = {
      val cds = cubeDataStoreRepo.create(dimensions, dataType)
      new DataStoreFactBackend(dataType, cds, aggregation)
    }
  }

  private case class FormulaFactBackend[T](dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]) extends FactBackend[T] {
    override def factType = FormulaFactBackend.key
    override val data = FormulaCube(formula, domain.cubes)
    override def editor = None
    def aggregation(aggregation: Aggregation[T]) = copy(aggregation = aggregation)
    def addDimension(moveTo: Coordinate) = throw new UnsupportedOperationException("cannot modify dimenesions")
    def removeDimension(keepAt: Coordinate) = throw new UnsupportedOperationException("cannot modify dimenesions")

    def config = Json.obj {
      "formula" -> jsonFormulaRepo.serialize(formula).valueOr(e => throw new IllegalStateException(e))
      "aggregation" -> aggregation.name
    }
  }
  private object FormulaFactBackend {
    val key = "formula"
    def apply[T](dataType: FudimDataType[T], config: JsValue) = {
      val backend = for {
        formula <- jsonFormulaRepo.parse(config \ "formula")
        aggregation <- aggregationFromJson(config \ "aggregation")
      } yield new FormulaFactBackend(dataType, formula.asInstanceOf[Formula[T]], aggregation.asInstanceOf[Aggregation[T]])
      backend.valueOr(e => throw new IllegalStateException(s"Cannot load formula-fact from config: $e"))
    }
  }
}
