package base

import java.sql.Connection

trait DatabaseRepo {
  protected def withConnection[A](f: Connection ⇒ A): A
}
