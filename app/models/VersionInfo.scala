package models

import org.joda.time.DateTime
import base._
import domain.Version

/** Additional information about a version. */
case class VersionInfo(version: Version, at: DateTime)

trait VersionRepo {
  def create(): Version @tx
  def infoFor(version: Version): VersionInfo
}
