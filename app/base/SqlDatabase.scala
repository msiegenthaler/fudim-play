package base

import java.sql.Connection

trait SqlDatabase extends TransactionState {
  protected def connection: Connection
}

object SqlDatabase extends TransactionalRessource {
  def transaction[A](b: Connection => A) = execute {
    case db: SqlDatabase => b(db.connection)
    case t => throw new IllegalStateException(s"Transaction $t does not support SqlDatabase")
  }
}
