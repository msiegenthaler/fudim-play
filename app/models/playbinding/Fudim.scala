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

  private val ids = new AtomicLong(1)

  private case class TxState(id: Long, connection: Connection) extends TransactionState {
    override def toString = s"tx-$id"
  }

  private[playbinding] object Db extends SqlDatabase {
    def transaction[A](b: (Connection) => A) = execute {
      case TxState(id, connection) => b(connection)
      case _ => throw new AssertionError("Wrong TxState, not a Db.")
    }
    def readOnly[A](b: (Connection) => A) = {
      //If a transaction is running on the thread, then reuse the tx's connection
      // else we get deadlocks and other not so nice things.
      threadTx.get.map { tx =>
        b(tx.connection)
      }.getOrElse {
        val c = DB.getConnection(autocommit = false)
        try {
          b(c)
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
}

trait FudimResources {
  protected def db: SqlDatabase = Fudim.Db
}
