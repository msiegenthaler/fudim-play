package models.db

import base._
import anorm._
import anorm.SqlParser._
import models.{FudimVersionInfo, FudimVersion, FudimVersionRepo}
import support.AnormDb
import org.joda.time._

trait DatabaseVersionRepo extends FudimVersionRepo {
  protected def database: SqlDatabase
  protected val db = new AnormDb(database)

  override def create() = {
    val id = db.insert(SQL("insert into version() values()"))
    FudimVersion(id.get)
  }

  def infoFor(version: FudimVersion) = {
    db.notx.select(SQL("select * from version where id = {id}").on("id" -> version.id), versionInfo singleOpt).
      getOrElse(throw new IllegalStateException(s"Version $version not found"))
  }

  private val versionInfo = long("id") ~ date("ts") map {
    case id ~ date => FudimVersionInfo(FudimVersion(id), new DateTime(date))
  }
}
