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
object CacheControl {
  def apply(v: Version): ActionBuilder[Request] = {
    val info = repo.infoFor(v)
    CacheControlBuilder(versionTag(v), info.at)
  }

  private def repo = DomainRepo.versionRepo

  private case class CacheControlBuilder(etag: EntityTag, date: DateTime) extends ActionBuilder[Request] {
    def invokeBlock[A](request: Request[A], block: (Request[A]) ⇒ Future[SimpleResult]) = {
      val notModified = request.headers.ifNoneMatch match {
        case Left(AllETags) ⇒ request.headers.ifModifiedSince.filter(_ > date).isEmpty
        case Right(etags) ⇒ etags.contains(etag)
      }
      val headers = CacheHeaders.lastModified(date) :: CacheHeaders.etag(etag) :: Nil
      if (notModified) {
        Future.successful(Results.NotModified.withHeaders(headers: _*))
      } else {
        block(request).map(_.withHeaders(headers: _*))
      }
    }
  }

  private def versionTag(v: domain.Version) = {
    val data = ByteBuffer.allocate(8).putLong(v.id).array()
    EntityTag.hashed(data, true)
  }
}

