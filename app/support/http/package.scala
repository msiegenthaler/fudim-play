package support

import play.api.mvc.Headers

package object http {
	implicit def headers2CacheHeaders(header: Headers) = CacheHeaders(header)
}