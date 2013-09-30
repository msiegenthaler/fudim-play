package models.playbinding

import java.util.concurrent.atomic.AtomicLong
import java.sql.Connection
import play.api.db._
import play.api.Play.current
import base._

object Fudim {
  def apply[A](tx: Transaction[A]): A = {
    val id = ids.incrementAndGet()
    val conn = DB.getConnection(s"tx-$id", false)
    try {
      val state = TxState(id, conn)
      val r = tx.run(state)
      conn.commit()
      r
    } catch {
      case e: Exception =>
        conn.rollback()
        throw e
    }
  }

  private val ids = new AtomicLong(1)

  private case class TxState(id: Long, connection: Connection) extends SqlDatabase {
    override def toString = s"tx-$id"
  }
}
