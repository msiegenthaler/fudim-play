package domain

import base._

/** Version with a strictly increasing id. */
case class Version(id: Long) extends Ordered[Version] {
  def or(other: Version) = if (this < other) other else this

  override def compare(that: Version) = id.compareTo(that.id)
  override def toString = s"#$id"
}
