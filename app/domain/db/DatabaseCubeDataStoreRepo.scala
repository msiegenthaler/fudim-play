package domain
package db

import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import base._
import cube._
import support.AnormDb

trait DatabaseCubeDataStoreRepo extends CopyableCubeDataStoreRepo {
  override type CDS[T] = DatabaseCubeDataStore[T]

  protected def database: SqlDatabase
  protected val db = new AnormDb(database)
  protected def dimensionRepo: DimensionRepository
  protected def dataTypeRepo: DataTypeRepository
  protected def storeTypes: Traversable[StoreDataType[_]]
  protected def versioner: Versioner

  private case class CubeDefinition(id: Long, typeName: String, version: Version) {
    def tableName = s"databaseCube_data_$id"
  }
  private val cubeDefinition = {
    long("id") ~ str("type") ~ long("version") map {
      case id ~ tpe ~ v ⇒ CubeDefinition(id, tpe, Version(v))
    }
  }

  protected object Types {
    def apply(name: String): StoreDataType[_] = {
      nameMapping.get(name).getOrElse(throw new IllegalArgumentException(s"No longer supported data type: $name"))
    }
    def apply[T](dataType: DataType[T]): StoreDataType[T] = {
      dataTypeMapping.get(dataType).map(_.asInstanceOf[StoreDataType[T]]).
        getOrElse(throw new IllegalArgumentException(s"Unsupported data type: $dataType"))
    }
    private lazy val dataTypeMapping = storeTypes.map(t ⇒ (t.dataType, t)).toMap[DataType[_], StoreDataType[_]]
    private lazy val nameMapping = storeTypes.map(t ⇒ (t.dataType.name, t)).toMap[String, StoreDataType[_]]
  }

  override def get(id: Long): Option[CDS[_]] = {
    db.notx.select(SQL("select * from databaseCube where id={id}").on("id" -> id), cubeDefinition.singleOpt).
      map(loadFromDefinition)
  }

  override def create[T](dims: Set[Dimension], dataType: DataType[T]): DatabaseCubeDataStore[T] @tx = {
    val version = versioner.version
    val storeType = Types(dataType)
    val id = db.insert(SQL("insert into databaseCube(type, version) values({type}, {version})").
      on(("type" -> storeType.name), ("version" -> version.id))).
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database"))
    val definition = CubeDefinition(id, storeType.name, version)

    val cdims = dims.mapTx { dim ⇒
      val dimId = db.insert(SQL("insert into databaseCube_dimension(cube, dimension) values ({cube}, {dimension})").on("cube" -> id, "dimension" -> dim.name)).get
      (dim, "dim_" + dimId)
    }.toMap
    val cds = DatabaseCubeDataStoreImpl[T](definition, storeType, cdims)
    cds.create
    cds
  }

  override def remove(id: Long) = {
    val definition = db.select(SQL("select * from databaseCube where id={id}").on("id" -> id), cubeDefinition.singleOpt)
    definition.mapTx { definition ⇒
      val cds = loadFromDefinition(definition)
      db.delete(SQL("delete from databaseCube_dimension where cube={id}").on("id" -> definition.id))
      db.delete(SQL("delete from databaseCube where id={id}").on("id" -> definition.id))
      cds.drop
    }
  }

  private def loadFromDefinition(definition: CubeDefinition): DatabaseCubeDataStoreImpl[_] = {
    val dimensions = db.notx.select(SQL("select id, dimension from databaseCube_dimension where cube={id}").on("id" -> definition.id),
      long("id") ~ str("dimension") *).map {
        case id ~ name ⇒
          val d: Dimension = dimensionRepo.get(name).getOrElse(throw new IllegalStateException(s"Could not find dimension $name"))
          (d, s"dim_$id")
      }.toMap
    val storeType = Types(definition.typeName)
    DatabaseCubeDataStoreImpl(definition, storeType, dimensions)
  }

  protected def copyData[T](from: DatabaseCubeDataStoreImpl[T], to: DatabaseCubeDataStoreImpl[T], pos: Point = Point.empty): Unit @tx = {
    val commonDims = from.dims.filter(v ⇒ to.dims.contains(v._1))
    val newDims = to.dims.filterNot(v ⇒ commonDims.contains(v._1))
    require(pos.defines(newDims.keys))
    val version = versioner.version
    val fixed = newDims.map(_._1).zipWithIndex.map(v ⇒ (s"f${v._2}", toParameterValue(pos.coordinate(v._1).get.id))).toSeq
    val oldFields = (List("content", "{version}") ++ commonDims.map(_._2) ++ fixed.map(_._1).map("{" + _ + "}")).mkString(",")
    val newFields = (List("content", "version") ++ commonDims.map(d ⇒ to.dims(d._1)) ++ newDims.map(_._2)).mkString(",")
    val restrictOn = pos.onlyOn(from.dims.keySet -- to.dims.keySet).coordinates.map(c ⇒ (from.dims(c.dimension), toParameterValue(c.id)))
    val where = restrictOn.map(v ⇒ s"${v._1} = {${v._1}}").mkString(" AND ")
    db.insert(SQL(s"INSERT INTO ${to.table}($newFields) SELECT $oldFields FROM ${from.table}" + (if (where.length > 0) s" WHERE $where" else "")).
      on(fixed ++ params(("version", version.id)) ++ restrictOn: _*))
  }

  private def params(args: (Any, ParameterValue[_])*): Seq[(Any, ParameterValue[_])] = args.toSeq

  def json = new JsonCubeDSMapper {
    import scalaz._
    import Scalaz.{ ToOptionOpsFromOption, ToValidationV }
    override val id = "databaseCubeDataStore"
    override def parser = json ⇒
      for {
        id ← (json \ "id").asOpt[Long].toSuccess("Missing value 'id'")
        cds ← get(id).toSuccess(s"Could not find CubeDataStore with id $id in database")
      } yield cds
    override def serializer = {
      case cds: DatabaseCubeDataStore[_] ⇒ Json.obj("id" -> cds.id).success
    }
  }

  private case class DatabaseCubeDataStoreImpl[T](definition: CubeDefinition, storeType: StoreDataType[T], dims: Map[Dimension, String])
    extends DatabaseCubeDataStore[T] with CoordinateFactory {

    override type Self = DatabaseCubeDataStoreImpl[T]
    override val id = definition.id
    val table = definition.tableName
    protected def repo = DatabaseCubeDataStoreRepo.this

    def create: Unit @tx = {
      val fields = s"content ${storeType.sqlType}" :: "version bigint not null" :: dims.values.map { d ⇒ s"$d integer not null" }.toList
      db.execute(SQL(s"CREATE TABLE $table (${fields.mkString(",")})"))
    }
    def drop: Unit @tx = db.execute(SQL(s"DROP TABLE $table"))

    override def copy(add: Point = Point.empty, remove: Point = Point.empty) = {
      val missing = remove.on.filterNot(dims.keySet.contains)
      require(missing.isEmpty, s"Dimension ${missing.mkString(",")} does not exist in $this")
      val already = dims.keySet.intersect(add.on)
      require(already.isEmpty, s"Dimensions ${already.mkString(",")} do already exist in $this")

      val newCube = cloneStructure(dims.keySet ++ add.on -- remove.on)
      copyData(this, newCube, add ++ remove)
      newCube
    }
    protected def cloneStructure(dims: Set[Dimension]): Self @tx = {
      val res = repo.create(dims, dataType)
      res.asInstanceOf[Self]
    }

    private def fromDb: RowParser[T] = storeType.fromDb("content")
    private def coordToDb(v: Coordinate): ParameterValue[Long] = v.id
    private def coordFromDb(d: Dimension, nameFromDims: String): RowParser[Coordinate] = long(nameFromDims).map(coordinate(d, _))
    private def mkWhere(p: Point): (String, Seq[(Any, ParameterValue[_])]) = {
      val vs = p.coordinates.map(e ⇒ (dims(e.dimension), e))
      val sql = vs.map(_._1).map(l ⇒ s"$l = {$l}").mkString(" AND ")
      val ons = vs.map(e ⇒ (e._1, coordToDb(e._2))).toSeq
      (sql, ons)
    }
    private def pointFromDb: RowParser[Point] = {
      dims.foldLeft(RowParser(_ ⇒ Success(Point.empty))) { (pp, d) ⇒
        pp >> (p ⇒ coordFromDb(d._1, d._2).map(p.+))
      }
    }

    private def insert(p: Point, value: T): Unit @tx = {
      val fields = p.on.map(dims.apply)
      val values = fields.map(f ⇒ s"{$f}")
      val version = versioner.version
      val ons = p.coordinates.map(e ⇒ (dims(e.dimension), coordToDb(e))).toSeq ++
        params(("content" -> storeType.toDb(value)), ("version" -> version.id))
      db.insert(
        SQL(s"INSERT INTO $table(content,version,${fields.mkString(",")}) VALUES ({content},{version},${values.mkString(",")})").on(ons: _*))
    }
    private def update(at: Point, value: T): Boolean @tx = {
      val (where, ons) = mkWhere(at)
      val version = versioner.version
      val cnt = db.update(SQL(s"UPDATE $table SET content={content},version={version} WHERE $where").
        on(ons ++ params(("content" -> storeType.toDb(value)), ("version" -> version.id)): _*))
      cnt match {
        case 1 ⇒ true
        case 0 ⇒ false
        case nr ⇒ throw new IllegalStateException(s"Too many rows affected by DatabaseCubeUpdate on $table ($nr rows)")
      }
    }
    private def delete(at: Point): Unit @tx = {
      val (where, ons) = mkWhere(at)
      db.delete(SQL(s"DELETE FROM $table WHERE $where").on(ons: _*))
    }

    private val cube_ = TheCube(id)
    override def cube: VersionedCube[T] = cube_
    private case class TheCube(id: Long, slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[T] with VersionedCube[T] {
      override type Self = TheCube

      override def allDimensions = dims.keys.toSet
      override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)

      override def get(at: Point) = {
        if (at.definesExactly(dims.keys) && matches(at)) {
          val (where, ons) = mkWhere(at)
          db.notx.select(SQL(s"SELECT content FROM $table WHERE $where").on(ons: _*), fromDb.singleOpt)
        } else None
      }
      override def sparse = {
        val as = (fromDb ~ pointFromDb).*
        val res = if (slice.on.isEmpty) {
          db.notx.select(SQL(s"SELECT * FROM $table"), as)
        } else {
          val (where, ons) = mkWhere(slice)
          db.notx.select(SQL(s"SELECT * FROM $table WHERE $where").on(ons: _*), as)
        }
        res.filter(e ⇒ matches(e._2)).map { case value ~ point ⇒ (point, value) }
      }
      override def dense = allPoints.map(p ⇒ (p, get(p)))
      override def allPoints = super.allPoints

      override def version = {
        val versionId = if (slice.on.isEmpty) {
          db.notx.single(SQL(s"SELECT max(version) FROM $table"), scalar[Option[Long]])
        } else {
          val (where, ons) = mkWhere(slice)
          db.notx.single(SQL(s"SELECT max(version) FROM $table WHERE $where").on(ons: _*), scalar[Option[Long]])
        }
        versionId.map(Version(_)).getOrElse(definition.version)
      }

      override def toString = s"DatabaseCube($id, slice=$slice, filters=$filters)"
    }

    override val editor: CubeEditor[T] = TheEditor(id)
    private case class TheEditor(id: Long) extends CubeEditor[T] {
      override def isSettable(at: Point) = at.definesExactly(dims.keys)
      override def set(at: Point, to: Option[T]) = {
        if (isSettable(at)) {
          if (to.isDefined) to.foreachTx { value ⇒
            if (!update(at, value)) insert(at, value)
            else noop
          }
          else {
            delete(at)
          }
        } else {
          noop
          throw new ValueCannotBeSetException(at)
        }
      }
      override def multiSet(filter: Point, value: Option[T]) = {
        cube_.slice(filter).allPoints.foreachTx(set(_, value))
      }
      override def toString = s"DatabaseCubeEditor($id)"
    }

    override def equals(o: Any) = o match {
      case other: DatabaseCubeDataStore[T] ⇒ other.id == id
      case _ ⇒ false
    }
    override def hashCode = id.hashCode
    override def toString = s"DatabaseCube($id)"
  }
}
