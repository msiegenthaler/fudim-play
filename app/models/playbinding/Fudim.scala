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
    val conn = DB.getConnection(autocommit = false)
    try {
      val state = TxState.full(conn)
      threadTx.set(Some(state))
      val (_, r) = tx.run(state)
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


  private case class TxState private(id: Long, isReadOnly: Boolean, connection: Connection) extends TransactionState {
    override def toString = {
      if (isReadOnly) s"ro-tx-$id"
      else s"tx-$id"
    }
  }
  private object TxState {
    private val ids = new AtomicLong(0)
    def full(c: Connection) = TxState(ids.incrementAndGet(), false, c)
    def readOnly(c: Connection) = TxState(ids.incrementAndGet(), true, c)
  }


  private[playbinding] object Db extends SqlDatabase {
    def inTransaction[A](b: (Connection) => A) = execute {
      case s@TxState(id, _, connection) => (s, b(connection))
      case _ => throw new AssertionError("Unsupported TransactionState: Does not implement Db")
    }
    def readOnly[A](tx: Transaction[A]) = threadTx.get match {
      //If a transaction is running on the thread, then reuse the tx's connection
      // else we get deadlocks and other not so nice things.
      case Some(runningTx) =>
        tx.run(runningTx)._2
      //No running tx on this thread.
      case nonRunningTx =>
        val c = DB.getConnection(autocommit = false)
        try {
          val state = TxState.readOnly(c)
          tx.run(state)._2
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
