package models.playbinding

import java.util.concurrent.atomic.AtomicLong
import java.sql.Connection
import play.api.db._
import play.api.Play.current
import base._

object Fudim {
  private val threadTx = new ThreadLocal[Option[TxState]] {
    override def initialValue = None
  }

  def exec[A](tx: Transaction[A]): A = {
    val id = ids.incrementAndGet()
    val conn = DB.getConnection(autocommit = false)
    try {
      val state = TxState(id, conn)
      threadTx.set(Some(state))
      val r = tx.run(state)
      conn.commit()
      r
    } catch {
      case e: Exception =>
        conn.rollback()
        throw e
    } finally {
      threadTx.remove()
      conn.close()
    }
  }
  def execTx[A](tx: => A@tx): A = exec(tx.transaction)

  private val ids = new AtomicLong(1)

  private case class TxState(id: Long, connection: Connection) extends TransactionState {
    override def toString = s"tx-$id"
  }
  private case class ReadOnlyTxState(connection: Connection) extends TransactionState {
    override def toString = "readonly"
  }

  private[playbinding] object Db extends SqlDatabase {
    def inTransaction[A](b: (Connection) => A) = execute {
      case TxState(id, connection) => b(connection)
      case ReadOnlyTxState(connection) => b(connection)
      case _ => throw new AssertionError("Wrong TxState, not a Db.")
    }
    def readOnly[A](tx: Transaction[A]) = threadTx.get match {
      //If a transaction is running on the thread, then reuse the tx's connection
      // else we get deadlocks and other not so nice things.
      case Some(runningTx) =>
        tx.run(runningTx)
      //No running tx on this thread.
      case nonRunningTx =>
        val c = DB.getConnection(autocommit = false)
        try {
          val state = new ReadOnlyTxState(c)
          tx.run(state)
        } finally {
          try {
            c.rollback()
          } finally {
            c.close()
          }
        }
    }
  }
}

trait FudimResources {
  protected def database: SqlDatabase = Fudim.Db
}
