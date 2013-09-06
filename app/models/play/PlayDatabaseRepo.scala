package models.play

import java.sql.Connection
import _root_.play.api.db._
import _root_.play.api.Play.current

trait PlayDatabaseRepo {
  def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
}