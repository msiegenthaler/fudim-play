package models.cube

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import CubeData._
import models._
import java.sql.Connection

object DatabaseCubeData {
  private case class CubeDefinition(id: Long, name: String, tpe: String) {
    def tableName = s"databaseCube_data_$id"
    def dimensionName(d: Dimension) = "dim_" + Dimension.idOf(d)
  }
  private val cubeDefinition = {
    get[Long]("id") ~
      get[String]("name") ~ get[String]("type") map {
        case id ~ name ~ tpe ⇒ CubeDefinition(id, name, tpe)
      }
  }

  def load[T](name: String, tpe: Class[T]): Option[EditableCubeData[T]] = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where name={name}").on("name" -> name).as(cubeDefinition.singleOpt).map { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      val cubeTypeExpect = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
      if (cubeType != cubeTypeExpect)
        throw new IllegalArgumentException(s"type of cube $name does not match: expected $cubeType but is $cubeTypeExpect")
      cube.asInstanceOf[DCDBase[T]]
    }
  }

  def create[T](name: String, dims: Set[Dimension], tpe: Class[T]): EditableCubeData[T] = DB.withConnection { implicit c ⇒
    if (SQL("select count(*) from databaseCube where name=>{name}").on("name" -> name).as(scalar[Long].single) > 0)
      throw new IllegalArgumentException(s"Cube $name already exists")

    val cubeType = typeMapping.get(tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube type: ${tpe.getName}"))
    val id = SQL("insert into databaseCube(name, type) value({name}, {type}").on("name" -> name, "type" -> cubeType.tpeName).executeInsert().
      getOrElse(throw new RuntimeException(s"Could not create the cube in the database ($name)"))
    val definition = CubeDefinition(id, name, cubeType.tpeName)

    val cdims = dims.map { dim ⇒
      SQL("insert into databaseCube_dimension(cube, dimension) values {cube}, dimension}").
        on("cube" -> id, "dimension" -> Dimension.idOf(dim)).executeInsert()
      (dim, definition.dimensionName(dim))
    }.toMap

    val cube = cubeType(name, cdims)
    cube.create
    cube.asInstanceOf[DCDBase[T]]
  }

  def delete(name: String) = DB.withConnection { implicit c ⇒
    SQL("select * from databaseCube where name={name}").on("name" -> name).as(cubeDefinition.singleOpt).foreach { definition ⇒
      val (cube, cubeType) = loadFromDefinition(definition)
      cube.drop
      SQL("delete from databaseCube_dimension where cube={id}").on("id" -> definition.id).executeUpdate
      SQL("delete from databaseCube where id={id}").on("id" -> definition.id).executeUpdate
    }
  }

  private def loadFromDefinition(definition: CubeDefinition)(implicit c: Connection) = {
    val dims = SQL("select d.id as id, d.name as name from databaseCube_dimension dcd inner join dimension d on d.id = dcd.dimension where dcd.cube={id}").on("id" -> definition.id).
      as(get[Long]("id") ~ get[String]("name") *).map {
        case id ~ name ⇒ (Dimension.get(name).get, s"dim_$id")
      }.toMap
    val cubeType = typeMapping.values.find(_.tpeName == definition.tpe).getOrElse(throw new IllegalArgumentException(s"unsupported cube db-type: ${definition.tpe}"))
    val cube = cubeType(definition.tableName, dims)
    (cube, cubeType)
  }
  private val typeMapping: Map[Class[_], CubeType] = {
    val list = DCDString :: Nil
    list.map(e ⇒ (e.tpeClass, e)).toMap
  }

  private trait CubeType {
    val tpeName: String
    val tpeClass: Class[_]
    def apply(table: String, dims: Map[Dimension, String]): DCDBase[_]
    override def toString = tpeName
  }

  private sealed trait DCDBase[D] extends EditableCubeData[D] with AbstractCubeData[D] {
    type self = DCDBase[D]
    def table: String
    def dims: Map[Dimension, String]

    protected def sqlType: String
    protected def fromDb(name: String): RowParser[D]
    protected def toDb(value: D): ParameterValue[_] = value

    def create(implicit c: Connection): Unit = {
      val fields = dims.values.map { d ⇒ s"$d varchar(1024) not null" }
      SQL(s"CREATE TABLE $table (${fields.mkString(",")}, content as $sqlType)").execute
    }
    def drop(implicit c: Connection): Unit = SQL(s"DROP TABLE $table").execute

    private def fromDb: RowParser[D] = fromDb("content")
    private def coordToDb(v: String): ParameterValue[String] = v
    private def coordFromDb(nameFromDims: String): RowParser[String] = str(nameFromDims)
    private def mkWhere(p: Point): (String, Seq[(Any, ParameterValue[_])]) = {
      val vs = p.values.map(e ⇒ (dims(e._1), e._2))
      val sql = vs.map(_._1).map(l ⇒ s"$l = {$l}").mkString(" AND ")
      val ons = vs.map(e ⇒ (e._1, coordToDb(e._2))).toSeq
      (sql, ons)
    }
    private def pointFromDb: RowParser[Point] = {
      dims.foldLeft(RowParser(_ ⇒ Success(Point.empty))) { (pp, d) ⇒
        pp >> (p ⇒ coordFromDb(d._2).map(p + (d._1, _)))
      }
    }

    private def insert(p: Point, value: D)(implicit c: Connection): Unit = {
      val fields = p.on.map(dims.apply)
      val values = fields.map(f ⇒ s"{$f}")
      val ons = p.values.map(e ⇒ (dims(e._1), coordToDb(e._2))).toSeq :+ ("content" -> toDb(value))
      SQL(s"INSERT INTO $table(content,${fields.mkString(",")} VALUES ({content},${values.mkString(",")})").
        on(ons: _*).execute
    }
    private def update(at: Point, value: D)(implicit c: Connection): Boolean = {
      val (where, ons) = mkWhere(at)
      val ons2 = ons :+ ("content" -> toDb(value))
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

    override def get(at: Point) = DB.withConnection { implicit c ⇒
      if (at.definesExactly(dims.keys) && matches(at)) {
        val (where, ons) = mkWhere(at)
        SQL(s"SELECT content FROM $table WHERE $where").on(ons: _*).as(fromDb.singleOpt)
      } else None
    }
    override def sparse = DB.withConnection { implicit c ⇒
      val as = fromDb ~ pointFromDb *
      val res = if (slice.on.isEmpty) {
        SQL(s"SELECT * FROM $table").as(as)
      } else {
        val (where, ons) = mkWhere(slice)
        SQL(s"SELECT * FROM $table WHERE $where").on(ons: _*).as(as)
      }
      res.filter(e ⇒ matches(e._2)).map { case value ~ point ⇒ (point, value) }
    }
    override def dense = allPoints.map(p ⇒ (p, get(p)))

    override def isSettable(at: Point) = at.definesExactly(dims.keys)
    override def set(at: Point, to: Option[D]) = DB.withConnection { implicit c ⇒
      if (isSettable(at)) {
        to match {
          case Some(value) ⇒ if (!update(at, value)) insert(at, value)
          case None ⇒ delete(at)
        }
      } else throw new ValueCannotBeSetException(at)
    }
    override def setAll(to: Option[D]) = allPoints.foreach(set(_, to))
  }

  private case class DCDString(table: String, dims: Map[Dimension, String], slice: Point = Point.empty, filters: DimensionFilter = Map.empty) extends DCDBase[String] {
    def sqlType = "varchar(1024)"
    def fromDb(name: String) = str(name)
    override def derive(slice: Point = slice, filters: DimensionFilter = filters) = copy(slice = slice, filters = filters)
  }
  private object DCDString extends CubeType {
    override val tpeName = "string"
    override val tpeClass = classOf[String]
    override def apply(table: String, dims: Map[Dimension, String]) = new DCDString(table, dims)
  }

}