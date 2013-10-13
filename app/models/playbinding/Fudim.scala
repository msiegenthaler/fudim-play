package models.playbinding

import scala.util.control.Exception._
import java.util.concurrent.atomic.AtomicLong
import java.sql.Connection
import play.api.db._
import play.api.Play.current
import base._
import play.api.Logger

object Fudim {
  def exec[A](tx: Transaction[A]): A = {
    val state = TxState.full
    val decTx = tx <* onEndOfTx
    val (s, r) = decTx.run(state)
    cleanupTx(s)
    r.fold(throw _, identity)
  }
  def execTx[A](tx: => A@tx): A = exec(tx.transaction)

  /** Executed at the end of the transaction (inside the transaction, only if transaction is successful). */
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
  }


  private[playbinding] object Db extends SqlDatabase {
    private[this] val connStack = new ThreadLocal[List[(Long, Connection)]] {
      override protected def initialValue = Nil
      def register(sid: Long, c: Connection) = set((sid -> c) :: get.filterNot(_._1 == sid))
      def cleanup(sid: Long) = set(get.filterNot(_._1 == sid))
      def top = get.headOption.map(_._2)
    }

    def inTransaction[A](b: Connection => A) = execute {
      case s@TxState(_, _, Some(conn)) =>
        (s, b(conn))
      case s@TxState(_, false, None) =>
        Logger.trace(s"Opening a new DB connection for transaction $s")
        val conn = DB.getConnection(autocommit = false)
        connStack.register(s.id, conn)
        (s.copy(connection = Some(conn)), b(conn))
      case s@TxState(_, true, None) =>
        val conn = {
          //If a transaction is running on the thread, then reuse the tx's connection
          // else we get deadlocks and other not so nice things.
          connStack.top.map { c =>
            Logger.trace(s"Reusing DB connection of running tx for $s")
            ReadOnlyConnection(c)
          }.getOrElse {
            Logger.trace(s"Opening a new read-only DB connection for transaction $s")
            val c = DB.getConnection(autocommit = false)
            c.setReadOnly(true)
            c
          }
        }
        (s.copy(connection = Some(conn)), b(conn))
      case _ => throw new AssertionError("Unsupported TransactionState: Does not implement Db")
    }

    def readOnly[A](tx: Transaction[A]) = {
      //If a transaction is running on the thread, then reuse the tx's connection
      // else we get deadlocks and other not so nice things.
      val state = TxState.readOnly
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
      case TxState(id, _, conn) => conn.foreach { conn =>
        connStack.cleanup(id)
        ignoring(classOf[Exception])(conn.rollback)
        ignoring(classOf[Exception])(conn.close)
      }
    }
  }
}

trait FudimResources {
  protected def database: SqlDatabase = Fudim.Db
}
