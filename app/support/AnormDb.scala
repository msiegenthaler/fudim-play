package support

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import base._

/** Helper for database handling with anorm. */
class AnormDb(db: SqlDatabase) {
  def apply[A](f: Connection => A) = db.transaction(f).tx
  def single[A](sql: Sql, parser: RowParser[A]) = apply(sql.single(parser)(_))
  def select[A](sql: Sql, parser: ResultSetParser[A]) = apply(sql.as(parser)(_))
  def insert[A](sql: Sql, parser: ResultSetParser[A] = scalar[Long].singleOpt) = apply(sql.executeInsert(parser)(_))
  def update(sql: Sql) = apply(sql.executeUpdate()(_))

}
