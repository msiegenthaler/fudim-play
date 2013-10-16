package domain

import base._

/** Version with a strictly increasing id. */
case class Version(id: Long) extends Ordered[Version] {
  override def compare(that: Version) = id.compareTo(that.id)
  override def toString = s"#$id"
}
