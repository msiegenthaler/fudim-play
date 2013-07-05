package models.cube.db

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import models._
import models.cube._

/**
 * Base class for a database cube.
 */
private trait DatabaseCubeBase[D] extends DatabaseCube[D] with AbstractCube[D] {
  protected override type Self <: DatabaseCubeBase[D]
  def table: String
  def dims: Map[Dimension, String]
  protected override def allDimensions = dims.keys.toSet

  protected def sqlType: String
  protected def fromDb(name: String): RowParser[D]
  protected def toDb(value: D): ParameterValue[_] = value
  protected def withConnection[A](f: Connection ⇒ A): A

  def create(implicit c: Connection): Unit = {
    val fields = dims.values.map { d ⇒ s"$d integer not null" }
    SQL(s"CREATE TABLE $table (${fields.mkString(",")}, content $sqlType)").execute
  }
  def drop(implicit c: Connection): Unit = SQL(s"DROP TABLE $table").execute

  private def fromDb: RowParser[D] = fromDb("content")
  private def coordToDb(v: Coordinate): ParameterValue[Long] = v.id
  private def coordFromDb(d: Dimension, nameFromDims: String): RowParser[Coordinate] = long(nameFromDims).map(new Coordinate(d, _))
  private def mkWhere(p: Point): (String, Seq[(Any, ParameterValue[_])]) = {
    val vs = p.coordinates.map(e ⇒ (dims(e.dimension), e))
    val sql = vs.map(_._1).map(l ⇒ s"$l = {$l}").mkString(" AND ")
    val ons = vs.map(e ⇒ (e._1, coordToDb(e._2))).toSeq
    (sql, ons)
  }
  private def pointFromDb: RowParser[Point] = {
    dims.foldLeft(RowParser(_ ⇒ Success(Point.empty))) { (pp, d) ⇒
      pp >> (p ⇒ coordFromDb(d._1, d._2).map(p + _))
    }
  }

  private def insert(p: Point, value: D)(implicit c: Connection): Unit = {
    val fields = p.on.map(dims.apply)
    val values = fields.map(f ⇒ s"{$f}")
    val ons = p.coordinates.map(e ⇒ (dims(e.dimension), coordToDb(e))).toSeq :+ ("content" -> toDb(value))
    SQL(s"INSERT INTO $table(content,${fields.mkString(",")}) VALUES ({content},${values.mkString(",")})").
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

  override def get(at: Point) = withConnection { implicit c ⇒
    if (at.definesExactly(dims.keys) && matches(at)) {
      val (where, ons) = mkWhere(at)
      SQL(s"SELECT content FROM $table WHERE $where").on(ons: _*).as(fromDb.singleOpt)
    } else None
  }
  override def sparse = withConnection { implicit c ⇒
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
  override def set(at: Point, to: Option[D]) = withConnection { implicit c ⇒
    if (isSettable(at)) {
      to match {
        case Some(value) ⇒ if (!update(at, value)) insert(at, value)
        case None ⇒ delete(at)
      }
    } else throw new ValueCannotBeSetException(at)
  }
  override def setAll(to: Option[D]) = allPoints.foreach(set(_, to))
}