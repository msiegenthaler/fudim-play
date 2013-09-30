package base

import java.sql.Connection

trait SqlDatabase extends TransactionState {
  protected def connection: Connection
}

object SqlDatabase extends TransactionalRessource[SqlDatabase] {
  def transaction[A](b: Connection => A) = execute(r => b(r.connection))
}
