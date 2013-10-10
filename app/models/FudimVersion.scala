package models

import base._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

/** Version with a strictly increasing id. */
class FudimVersion(val id: Long, val at: DateTime) extends Ordered[FudimVersion] {
  override def compare(that: FudimVersion) = id.compareTo(that.id)

  override def equals(o: Any) = o match {
    case o: FudimVersion => id == o.id
    case _ => false
  }
  override def hashCode = id.hashCode
  override def toString = {
    val dt = ISODateTimeFormat.dateTime().print(at)
    s"Version $id ($dt)"
  }
}

trait FudimVersionRepo {
  def create(): FudimVersion@tx
}
