package models
package db

import scalaz._
import Scalaz._
import anorm._
import anorm.SqlParser._
import base._
import cube._
import domain._
import play.api.Logger
import play.api.libs.json._
import support.AnormDb

trait DatabaseFactRepo extends FudimFactRepo {
  protected def database: SqlDatabase
  protected val db = new AnormDb(database)
  protected def domain: FudimDomain
  protected def dataTypeRepo = FudimDataTypes
  protected def jsonFormulaRepo: JsonFormulaMapperRepository
  protected def cubeDataStoreRepo: CopyableCubeDataStoreRepo

  override def get(name: String) = getInternal(name)
  protected def getInternal(name: String): Option[DatabaseFact[_]] = {
    db.notx.select(
      SQL("select * from fact where domain={domain} and name={name}").on("domain" -> domain.id.id, "name" -> name),
      fact singleOpt).flatten
  }

  override def all = {
    db.notx.select(
      SQL("select * from fact where domain={domain}").on("domain" -> domain.id.id),
      fact *).flatten
  }

  def createDatabaseBacked[T](name: String, dataType: FudimDataType[T], dimensions: Set[Dimension], aggregation: Aggregation[T]) = {
    val backend = DataStoreFactBackend(dataType, dimensions, aggregation)
    create(name, backend)
  }
  def createFormulaBased[T](name: String, dataType: FudimDataType[T], formula: Formula[T], aggregation: Aggregation[T]) = {
    val backend = FormulaFactBackend(dataType, formula, aggregation)
    create(name, backend)
  }
  protected def create[T](name: String, backend: FactBackend[T]): DatabaseFact[T]@tx = {
    val config = Json.stringify(backend.config)
    val id = db.insert(SQL("insert into fact(domain, name, dataType, factType, config) values({domain}, {name}, {dataType}, {factType}, {config})").
      on("domain" -> domain.id.id, "name" -> name, "dataType" -> backend.dataType.name, "factType" -> backend.factType, "config" -> config)).get
    new DatabaseFact(id, name, backend)
  }

  def remove(name: String) = {
    val fact = getInternal(name).tx
    fact.mapTx { fact =>
      fact.delete()
      db.delete(SQL("delete from fact where id={id}").on("id" -> fact.id))
    }.getOrElse(Transaction.empty)
  }

  private val fact = {
    long("id") ~ str("name") ~ str("dataType") ~ str("factType") ~ str("config") map {
      case id ~ name ~ dataTypeName ~ factType ~ config ⇒
        val fact: Validation[String, DatabaseFact[_]] = for {
          dataType ← dataTypeRepo.get(dataTypeName).toSuccess(s"DataType $dataTypeName is not known")
          json = Json.parse(config)
          backend <- {
            val mk: PartialFunction[String, FactBackend[_]] = {
              case DataStoreFactBackend.key => DataStoreFactBackend(dataType, json)
              case FormulaFactBackend.key => FormulaFactBackend(dataType, json)
            }
            mk.lift(factType).toSuccess(s"Unknown fact type $factType")
          }
        } yield new DatabaseFact(id, name, backend)
        fact.leftMap(msg ⇒ Logger.warn(s"Could not load fact $name: $msg"))
        fact.toOption
    }
  }

  protected final class DatabaseFact[T](val id: Long, val name: String, ib: FactBackend[T]) extends FudimFact[T] {
    private[this] var _backend: FactBackend[T] = ib
    def backend = synchronized(_backend)
    def updateBackend(nb: FactBackend[T]): Unit@tx = {
      val config = Json.stringify(nb.config)
      db.update(SQL("update fact set config={config} where id={id}").on("config" -> config, "id" -> id))
    }


    override def dataType = backend.dataType
    def data = backend.data
    def editor = backend.editor
    def aggregation = backend.aggregation
    def aggregation_=(aggr: Aggregation[T]) = updateBackend(backend.aggregation(aggregation = aggr))
    def addDimension(moveTo: Coordinate) = updateBackend(backend.addDimension(moveTo))
    def removeDimension(keepAt: Coordinate) = updateBackend(backend.removeDimension(keepAt))
    def delete() = backend.delete

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
    def addDimension(moveTo: Coordinate): FactBackend[T]@tx
    def removeDimension(keepAt: Coordinate): FactBackend[T]@tx
    def delete(): Unit@tx = noop
  }

  private def aggregationFromJson(json: JsValue) = for {
    aggregationName <- json.asOpt[String].toSuccess("Missing aggregation in config")
    aggregation <- Aggregation.all.find(_.name == aggregationName).toSuccess(s"Aggregation $aggregationName does not exist")
  } yield aggregation

  private case class DataStoreFactBackend[T](dataType: FudimDataType[T], cds: CopyableCubeDataStore[T], aggregation: Aggregation[T]) extends FactBackend[T] {
    override def factType = DataStoreFactBackend.key
    override val data = aggregation.aggregator.map(CubeDecorator(cds.cube, _)).getOrElse(cds.cube)
    override val editor = Some(cds.editor)

    override def aggregation(aggregation: Aggregation[T]) = copy(aggregation = aggregation)
    override def addDimension(moveTo: Coordinate) = {
      val ncds = cds.copy(moveTo, Point.empty)
      delete()
      copy(cds = ncds)
    }
    override def removeDimension(keepAt: Coordinate) = {
      val ncds = cds.copy(Point.empty, keepAt)
      delete()
      copy(cds = ncds)
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
        cds <- cubeDataStoreRepo.get(id, dataType).toSuccess(s"CubeDataStore $id not found")
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
    override val data = {
      val cube = FormulaCube(formula, domain.cubes)
      aggregation.aggregator.map(CubeDecorator(cube, _)).getOrElse(cube)
    }
    override def editor = None
    def aggregation(aggregation: Aggregation[T]) = copy(aggregation = aggregation)
    def addDimension(moveTo: Coordinate) = throw new UnsupportedOperationException("cannot modify dimenesions")
    def removeDimension(keepAt: Coordinate) = throw new UnsupportedOperationException("cannot modify dimenesions")

    def config = Json.obj(
      "formula" -> jsonFormulaRepo.serialize(formula).valueOr(e => throw new IllegalStateException(e)),
      "aggregation" -> aggregation.name
    )
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
