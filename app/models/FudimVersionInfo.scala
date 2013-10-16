package models

import org.joda.time.DateTime
import base._
import domain.Version

/** Additional information about a version. */
case class FudimVersionInfo(version: Version, at: DateTime)

trait FudimVersionRepo {
  def create(): Version @tx
  def infoFor(version: Version): FudimVersionInfo
}
