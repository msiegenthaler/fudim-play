package support

import java.sql.Connection
import anorm._
import anorm.SqlParser._
import base._

/** Helper for database handling with anorm. */
class AnormDb(db: SqlDatabase) {
  def single[A](sql: Sql, parser: RowParser[A]) = appl(sql.single(parser)(_))
  def select[A](sql: Sql, parser: ResultSetParser[A]) = appl(sql.as(parser)(_))

  object notx {
    private val o = AnormDb.this
    def single[A](sql: Sql, parser: RowParser[A]): A = db.readOnly(o.single(sql, parser).m)
    def select[A](sql: Sql, parser: ResultSetParser[A]): A = db.readOnly(o.select(sql, parser).m)
  }

  def insert[A](sql: Sql, parser: ResultSetParser[A] = scalar[Long].singleOpt) = appl(sql.executeInsert(parser)(_))
  def update(sql: Sql) = appl(sql.executeUpdate()(_))
  def delete(sql: Sql) = update(sql)
  def execute(sql: Sql) = appl(sql.execute()(_))

  private def appl[A](f: Connection => A) = db.inTransaction(f).tx
}
