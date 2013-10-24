package support.http

import org.joda.time.DateTime
import play.api.mvc.Headers
import play.api.http.HeaderNames._
import play.api.libs.Codecs

case class CacheHeaders(headers: Headers) {
  def ifNoneMatch: Either[AllETags.type, Set[EntityTag]] = {
    val values = headers.getAll(IF_NONE_MATCH).mkString(", ")
    val etagStrings = values.split(',').map(_.trim).filter(_.nonEmpty)
    if (etagStrings.contains("*")) Left(AllETags)
    else Right(etagStrings.flatMap(EntityTag.parse).toSet)
  }

  /** True if the If-None-Match header contains the etag. */
  def matchesEtag(etag: EntityTag): Boolean = ???

  def ifModifiedSince: Option[DateTime] = ???
}

case object AllETags

/**
 * HTTP-EntityTag (also called ETag).
 * An opaque identifier assigned by a web server to a specific version of a resource found at a URL
 */
case class EntityTag(id: String, isWeak: Boolean = false) {
  def value = {
    if (isWeak) "W/\"" + id + '"'
    else "\"" + id + "\""
  }
  override def toString = value
}
object EntityTag {
  def parse(string: String): Option[EntityTag] = {
    pattern.findFirstMatchIn(string).map { m ⇒
      EntityTag(m.group(2), m.group(1) != null)
    }
  }
  private val pattern = """^\w*(W/)*"([0-9a-zA-Z]*)"\w*""".r

  def hashed(value: String, weak: Boolean = false): EntityTag = hashed(value.getBytes("UTF-8"), weak)
  def hashed(value: Array[Byte], weak: Boolean): EntityTag = {
    val id = Codecs.md5(value)
    EntityTag(id, weak)
  }
}