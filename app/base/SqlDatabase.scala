package base

import java.sql.Connection

trait SqlDatabase extends TransactionalRessource {
  def transaction[A](b: Connection => A): Transaction[A]
  def readOnly[A](b: Connection => A): A
}
