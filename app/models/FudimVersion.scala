package models

import base._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

/** Version with a strictly increasing id. */
case class FudimVersion(id: Long) extends Ordered[FudimVersion] {
  override def compare(that: FudimVersion) = id.compareTo(that.id)
  override def toString = s"#$id"
}

case class FudimVersionInfo(version: FudimVersion, at: DateTime)

trait FudimVersionRepo {
  def create(): FudimVersion@tx
  def infoFor(version: FudimVersion): FudimVersionInfo
}
