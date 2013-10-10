package models.db

import base._
import anorm._
import anorm.SqlParser._
import models.{FudimVersion, FudimVersionRepo}
import support.AnormDb
import org.joda.time._

trait DatabaseVersionRepo extends FudimVersionRepo {
  protected def database: SqlDatabase
  protected val db = new AnormDb(database)

  override def create() = {
    db.insert(SQL("insert into version() values()")).flatMap(get).
      getOrElse(throw new IllegalStateException("Insert failed"))
  }

  private def get(id: Long): Option[FudimVersion] = {
    db.notx.select(SQL("select * from version where id = {id}").on("id" -> id), version singleOpt)
  }

  private val version = long("id") ~ date("ts") map {
    case id ~ date => new FudimVersion(id, new DateTime(date))
  }
}
