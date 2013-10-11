package models.playbinding

import scala.util.control.Exception._
import java.util.concurrent.atomic.AtomicLong
import java.sql.Connection
import play.api.db._
import play.api.Play.current
import base._

object Fudim {
  def exec[A](tx: Transaction[A]): A = {
    val state = TxState.full
    val decTx = tx <* onEndOfTx
    val (s, r) = tx.run(state)
    cleanupTx(s)
    r.fold(throw _, identity)
  }
  def execTx[A](tx: => A@tx): A = exec(tx.transaction)

  /** Executed at the end of the transaction (inside the transaction. */
  private def onEndOfTx = Db.commit
  /** Executed after the transaction. */
  private def cleanupTx(s: TransactionState) = Db.cleanupTx(s)


  private case class TxState private(id: Long, isReadOnly: Boolean, connection: Option[Connection]) extends TransactionState {
    override def toString = {
      if (isReadOnly) s"ro-tx-$id"
      else s"tx-$id"
    }
  }
  private object TxState {
    private val ids = new AtomicLong(0)
    def full = TxState(ids.incrementAndGet(), false, None)
    def readOnly = TxState(ids.incrementAndGet(), true, None)
    def readOnly(c: Connection) = TxState(ids.incrementAndGet(), true, Some(c))
  }


  private[playbinding] object Db extends SqlDatabase {
    private val txConn = new ThreadLocal[Option[Connection]] {
      override def initialValue = None
    }

    def inTransaction[A](b: (Connection) => A) = {
      def run(c: Connection) = {
        //makes sure that a readonly tx running inside the tx will reuse the connection (see readOnly).
        txConn.set(Some(c))
        try {
          b(c)
        } finally {
          txConn.set(None)
        }
      }
      execute {
        case s@TxState(_, _, Some(conn)) => (s, run(conn))
        case s@TxState(_, _, None) =>
          val conn = DB.getConnection(autocommit = false)
          (s.copy(connection = Some(conn)), run(conn))
        case _ => throw new AssertionError("Unsupported TransactionState: Does not implement Db")
      }
    }

    def readOnly[A](tx: Transaction[A]) = {
      //If a transaction is running on the thread, then reuse the tx's connection
      // else we get deadlocks and other not so nice things.
      val state = txConn.get.map(TxState.readOnly(_)).getOrElse(TxState.readOnly)

      val (s, result) = (tx <* rollback).run(state)
      Fudim.cleanupTx(s)
      result.fold(throw _, identity)
    }

    private[Fudim] def rollback = withExistingConnection(_.rollback())
    private[Fudim] def commit = withExistingConnection(_.commit())
    private def withExistingConnection[A](f: Connection => A) = execute {
      case s@TxState(_, _, Some(conn)) =>
        f(conn)
        (s, ())
      case s@TxState(_, _, None) => (s, ())
    }

    private[Fudim] def cleanupTx(s: TransactionState) = s match {
      case TxState(_, _, conn) => conn.foreach { conn =>
        catching(classOf[Exception]).andFinally(conn.rollback)
        catching(classOf[Exception]).andFinally(conn.close)
      }
    }
  }
}

trait FudimResources {
  protected def database: SqlDatabase = Fudim.Db
}
