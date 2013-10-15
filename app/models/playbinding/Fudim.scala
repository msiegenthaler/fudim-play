package models.playbinding

import scala.util.control.Exception._
import java.util.concurrent.atomic.AtomicLong
import java.sql.Connection
import play.api.db._
import play.api.Play.current
import base._
import play.api.Logger
import models.{FudimVersion}
import domain.VersionerState

object Fudim {
  def exec[A](tx: Transaction[A]): A = {
    val state = TxState.full
    val decTx = tx <* Db.commit
    val (s, r) = decTx.run(state)
    cleanupTx(s)
    r.fold(throw _, identity)
  }
  def execTx[A](tx: => A@tx): A = exec(tx.transaction)

  def execReadOnly[A](tx: Transaction[A]): A = {
    val (s, result) = (tx <* Db.rollback).run(TxState.readOnly)
    cleanupTx(s)
    result.fold(throw _, identity)
  }
  def execReadOnlyTx[A](tx: => A@tx): A = execReadOnly(tx.transaction)

  /** Executed after the transaction. */
  private def cleanupTx(s: TransactionState) = Db.cleanupTx(s)

  private case class TxState private(id: Long, isReadOnly: Boolean, connection: Option[Connection] = None, version: Option[FudimVersion] = None)
    extends TransactionState with VersionerState[FudimVersion] {

    def withVersion(version: Option[FudimVersion]) = copy(version = version)
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

    private def run[A](f: Connection => A, c: Connection): Either[Exception, A] = {
      try {
        Right(f(c))
      }
      catch {
        case e: Exception => Left(e)
      }
    }
    override def inTransaction[A](b: Connection => A) = execute {
      case s@TxState(_, _, Some(conn), _) =>
        (s, run(b, conn))
      case s@TxState(_, false, None, _) =>
        Logger.trace(s"Opening a new DB connection for transaction $s")
        val conn = DB.getConnection(autocommit = false)
        connStack.register(s.id, conn)
        (s.copy(connection = Some(conn)), run(b, conn))
      case s@TxState(_, true, None, _) =>
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
        (s.copy(connection = Some(conn)), run(b, conn))
      case _ => throw new AssertionError("Unsupported TransactionState: Does not implement Db")
    }

    override def readOnly[A](tx: Transaction[A]) = Fudim.execReadOnly(tx)

    private[Fudim] def rollback = withExistingConnection(_.rollback())
    private[Fudim] def commit = withExistingConnection(_.commit())
    private def withExistingConnection[A](f: Connection => A) = executeSafe {
      case s@TxState(_, _, Some(conn), _) => f(conn); ()
      case s@TxState(_, _, None, _) => ()
    }

    private[Fudim] def cleanupTx(s: TransactionState) = s match {
      case TxState(id, _, conn, _) => conn.foreach { conn =>
        connStack.cleanup(id)
        ignoring(classOf[Exception])(conn.rollback())
        ignoring(classOf[Exception])(conn.close())
      }
    }
  }
}

trait FudimResources {
  protected def database: SqlDatabase = Fudim.Db
}
