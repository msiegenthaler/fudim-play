package support

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import java.nio.ByteBuffer
import com.github.nscala_time.time.Imports._
import play.api._
import play.api.http.HeaderNames._
import play.api.mvc._
import domain.Version
import models.playbinding.DomainRepo
import support.http._

/** Sets the HTTP cache header and can shortcut request handling to respond with a NotModified. */
object HttpCache {
  def cached(request: { def headers: Headers }, v: Version)(block: ⇒ Result): Result = {
    val info = repo.infoFor(v)
    val etag = versionTag(v)
    val date = info.at

    val notModified = request.headers.ifNoneMatch match {
      case Left(AllETags) ⇒ request.headers.ifModifiedSince.filter(_ > date).isEmpty
      case Right(etags) ⇒ etags.contains(etag)
    }
    val headers = CacheHeaders.lastModified(date) :: CacheHeaders.etag(etag) :: Nil
    if (notModified) {
      Results.NotModified.withHeaders(headers: _*)
    } else {
      block match {
        case r @ SimpleResult(h, _, _) if h.status == 200 ⇒
          r.withHeaders(headers: _*)
        case other ⇒ other
      }
    }

  }
  private def repo = DomainRepo.versionRepo
  private def versionTag(v: domain.Version) = {
    val data = ByteBuffer.allocate(8).putLong(v.id).array()
    EntityTag.hashed(data, true)
  }
}

