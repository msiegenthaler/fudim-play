package models.dbcube

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import cube._
import domain._
import support.DatabaseRepo


trait DatabaseCubeDataStoreRepo extends DatabaseRepo {
  protected def dimensionRepo: DimensionRepository
  protected def dataTypeRepo: DataTypeRepository
  protected def storeTypes: Traversable[StoreDataType[_]]


  private case class CubeDefinition(id: Long, typeName: String) {
    def tableName = s"databaseCube_data_$id"
  }
  private val cubeDefinition = {
    get[Long]("id") ~ get[String]("type") map {
      case id ~ tpe ⇒ CubeDefinition(id, tpe)
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
    private lazy val dataTypeMapping = storeTypes.map(t => (t.dataType, t)).toMap[DataType[_], StoreDataType[_]]
    private lazy val nameMapping = storeTypes.map(t => (t.dataType.name, t)).toMap[String, StoreDataType[_]]
  }

  def load[T](id: Long, dataType: DataType[T]): Option[DatabaseCubeDataStore[T]] = load(id).map { cds =>
    if (cds.dataType != dataType)
      throw new IllegalArgumentException(s"type of cube $id (type ${cds.dataType} does not match expected type $dataType")
    cds.asInstanceOf[DatabaseCubeDataStore[T]]
  }
  def load[T](id: Long): Option[DatabaseCubeDataStore[_]] = withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> id).as(cubeDefinition.singleOpt).map { definition ⇒
      loadFromDefinition(definition)
    }
  }

  def create[T](dims: Set[Dimension], dataType: DataType[T]): DatabaseCubeDataStore[T] = withConnection { implicit c ⇒
    val storeType = Types(dataType)
    val id = SQL("insert into databaseCube(type) values({type})").on("type" -> storeType.name).executeInsert().
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database"))
    val definition = CubeDefinition(id, storeType.name)

    val cdims = dims.map { dim ⇒
      val dimId = SQL("insert into databaseCube_dimension(cube, dimension) values ({cube}, {dimension})").
        on("cube" -> id, "dimension" -> dim.name).executeInsert().map("dim_" + _).get
      (dim, dimId)
    }.toMap
    val cds = DatabaseCubeDataStoreImpl[T](definition, storeType, cdims)
    cds.create()
    cds
  }

  def delete(cube: DatabaseCubeDataStore[_]) = withConnection { implicit c ⇒
    SQL("select * from databaseCube where id={id}").on("id" -> cube.id).as(cubeDefinition.singleOpt).foreach { definition ⇒
      val cds = loadFromDefinition(definition)
      cds.drop()
      SQL("delete from databaseCube_dimension where cube={id}").on("id" -> definition.id).executeUpdate
      SQL("delete from databaseCube where id={id}").on("id" -> definition.id).executeUpdate
    }
  }

  private def loadFromDefinition(definition: CubeDefinition)(implicit c: Connection) = {
    val dimensions = SQL("select id, dimension from databaseCube_dimension where cube={id}").on("id" -> definition.id).
      as(get[Long]("id") ~ get[String]("dimension") *).map {
      case id ~ name ⇒
        val d: Dimension = dimensionRepo.get(name).getOrElse(throw new IllegalStateException(s"Could not find dimension $name"))
        (d, s"dim_$id")
    }.toMap
    val storeType = Types(definition.typeName)
    DatabaseCubeDataStoreImpl(definition, storeType, dimensions)
  }

  override protected[dbcube] def withConnection[A](f: (Connection) => A): A

  def json = new JsonCubeMapper {
    import scalaz._
    import Scalaz._
    override val id = "databaseCube"
    override def parser = json ⇒
      for {
        id ← (json \ "id").asOpt[Long].toSuccess("Missing value 'id'")
        cube ← load(id).toSuccess(s"Could not find cube with id $id in database")
      } yield cube.cube
    override def serializer = {
      case cube: DatabaseCubeDataStore[_] ⇒ Json.obj("id" -> cube.id).success
    }
  }

  private case class DatabaseCubeDataStoreImpl[T](definition: CubeDefinition, storeType: StoreDataType[T], dims: Map[Dimension, String]) extends DatabaseCubeDataStore[T] with CoordinateFactory with DatabaseRepo {
    override val id = definition.id
    override val table = definition.tableName

    def create(): Unit = withConnection { implicit c ⇒
      val fields = s"content ${storeType.sqlType}" :: dims.values.map { d ⇒ s"$d integer not null"}.toList
      SQL(s"CREATE TABLE $table (${fields.mkString(",")})").execute
    }
    def drop(): Unit = withConnection { implicit c ⇒
      SQL(s"DROP TABLE $table").execute
    }

    override def copyAndAddDimension(moveTo: Coordinate) = withConnection { implicit c ⇒
      val newDimension = moveTo.dimension
      if (dims.contains(newDimension)) throw new IllegalArgumentException(s"$this already contains dimension $newDimension")
      val newCube = cloneWithoutData(dims.keySet + newDimension)
      val newDim = newCube.dims(newDimension)
      val oldFields = ("content" :: dims.map(_._2).toList).mkString(",")
      val newFields = ("content" :: dims.map(d ⇒ newCube.dims(d._1)).toList).mkString(",")
      SQL(s"INSERT INTO ${newCube.table} ($newFields, $newDim) SELECT $oldFields, {d} FROM $table").
        on("d" -> moveTo.id).executeUpdate
      newCube
    }
    override def copyAndRemoveDimension(keepAt: Coordinate) = withConnection { implicit c ⇒
      val droppedDimension = keepAt.dimension
      if (!dims.contains(droppedDimension)) throw new IllegalArgumentException(s"$this does not contains dimension $droppedDimension")
      val newCube = cloneWithoutData(dims.keySet - droppedDimension)
      val droppedDim = dims(droppedDimension)
      val oldFields = ("content" :: newCube.dims.map(d ⇒ dims(d._1)).toList).mkString(",")
      val newFields = ("content" :: newCube.dims.map(_._2).toList).mkString(",")
      SQL(s"INSERT INTO ${newCube.table} ($newFields) SELECT $oldFields FROM $table WHERE $droppedDim = {d}").
        on("d" -> keepAt.id).executeUpdate
      newCube
    }
    protected def cloneWithoutData(dims: Set[Dimension]) = {
      val repo = DatabaseCubeDataStoreRepo.this
      repo.create(dims, dataType) match {
        case c: DatabaseCubeDataStoreImpl[T] => c
      }
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

    private def insert(p: Point, value: T)(implicit c: Connection): Unit = {
      val fields = p.on.map(dims.apply)
      val values = fields.map(f ⇒ s"{$f}")
      val ons = p.coordinates.map(e ⇒ (dims(e.dimension), coordToDb(e))).toSeq :+ ("content" -> storeType.toDb(value))
      SQL(s"INSERT INTO $table(content,${fields.mkString(",")}) VALUES ({content},${values.mkString(",")})").
        on(ons: _*).execute
    }
    private def update(at: Point, value: T)(implicit c: Connection): Boolean = {
      val (where, ons) = mkWhere(at)
      val ons2 = ons :+ ("content" -> storeType.toDb(value))
      SQL(s"UPDATE $table SET content={content} WHERE $where").on(ons2: _*).executeUpdate match {
        case 1 ⇒ true
        case 0 ⇒ false
        case nr ⇒ throw new IllegalStateException(s"Too many rows affected by DatabaseCubeUpdate on $table ($nr rows)")
      }
    }
    private def delete(at: Point)(implicit c: Connection): Unit = {
      val (where, ons) = mkWhere(at)
      SQL(s"DELETE FROM $table WHERE $where").on(ons: _*).executeUpdate
    }


    private val cube_ = TheCube(id)
    override val cube: Cube[T] = cube_
    private case class TheCube(id: Long, slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends AbstractCube[T] {
      override type Self = TheCube

      override def allDimensions = dims.keys.toSet
      override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)

      override def get(at: Point) = withConnection { implicit c ⇒
        if (at.definesExactly(dims.keys) && matches(at)) {
          val (where, ons) = mkWhere(at)
          SQL(s"SELECT content FROM $table WHERE $where").on(ons: _*).as(fromDb.singleOpt)
        } else None
      }
      override def sparse = withConnection { implicit c ⇒
        val as = (fromDb ~ pointFromDb).*
        val res = if (slice.on.isEmpty) {
          SQL(s"SELECT * FROM $table").as(as)
        } else {
          val (where, ons) = mkWhere(slice)
          SQL(s"SELECT * FROM $table WHERE $where").on(ons: _*).as(as)
        }
        res.filter(e ⇒ matches(e._2)).map { case value ~ point ⇒ (point, value)}
      }
      override def dense = allPoints.map(p ⇒ (p, get(p)))
      override def allPoints = super.allPoints
      override def toString = s"DatabaseCube($id, slice=$slice, filters=$filters)"
    }

    override val editor: CubeEditor[T] = TheEditor(id)
    private case class TheEditor(id: Long) extends CubeEditor[T] {
      override def isSettable(at: Point) = at.definesExactly(dims.keys)
      override def set(at: Point, to: Option[T]) = withConnection { implicit c ⇒
        if (isSettable(at)) {
          to match {
            case Some(value) ⇒ if (!update(at, value)) insert(at, value)
            case None ⇒ delete(at)
          }
        } else throw new ValueCannotBeSetException(at)
      }
      override def multiSet(filter: Point, value: Option[T]) = cube_.slice(filter).allPoints.foreach(set(_, value))
      override def toString = s"DatabaseCubeEditor($id)"
    }

    override def equals(o: Any) = o match {
      case other: DatabaseCubeDataStore[T] => other.id == id
      case _ => false
    }
    override def hashCode = id.hashCode
    override def toString = s"DatabaseCube($id)"
  }
}
