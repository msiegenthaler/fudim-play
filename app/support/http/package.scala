package support

import play.api.mvc.Headers

package object http {
  type HeaderSupplier = {
    def get(name: String): Option[String]
    def getAll(name: String): Seq[String]
  }
  implicit def headers2CacheHeaders(header: Headers) = CacheHeaders(header)
}