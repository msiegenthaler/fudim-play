package base

import java.sql.Connection

trait SqlDatabase extends TransactionalRessource {
  def inTransaction[A](b: Connection => A): Transaction[A]
  def readOnly[A](tx: Transaction[A]): A
}
