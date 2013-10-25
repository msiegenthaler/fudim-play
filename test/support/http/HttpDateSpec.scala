package support.http

import org.specs2.mutable.Specification
import org.joda.time.DateTime
import org.joda.time.DateTimeZone

class HttpDateSpec extends Specification {
  "HttpDate.parse" should {
    "successfully parse 'Wed, 09 Jun 2021 10:18:14 GMT'" in {
      val dt = new DateTime(2021, 6, 9, 10, 18, 14, DateTimeZone.forID("UTC"))
      HttpDate.parse("Wed, 09 Jun 2021 10:18:14 GMT") must beSome(dt)
    }
    "successfully parse 'Wed, 13-Jan-2021 22:23:01 GMT'" in {
      val dt = new DateTime(2021, 1, 13, 22, 23, 1, DateTimeZone.forID("UTC"))
      HttpDate.parse("Wed, 13-Jan-2021 22:23:01 GMT") must beSome(dt)
    }
    "handle explicit UTC timezone" in {
      val dt = new DateTime(1994, 11, 6, 8, 49, 37, DateTimeZone.forID("UTC"))
      HttpDate.parse("Sun, 06 Nov 1994 08:49:37 UTC") must beSome(dt)
    }
    "handle explicit ETC timezone" in {
      val dt = new DateTime(1994, 11, 6, 13, 49, 37, DateTimeZone.forID("UTC"))
      HttpDate.parse("Sun, 06 Nov 1994 08:49:37 EST") must beSome(dt)
    }
    "handle explicit +02:00 timezone" in {
      val dt = new DateTime(1994, 11, 6, 6, 49, 37, DateTimeZone.forID("UTC"))
      HttpDate.parse("Sun, 06 Nov 1994 08:49:37 +02:00") must beSome(dt)
    }
    "correctly handle implicit utc" in {
      val dt = new DateTime(1994, 11, 6, 8, 49, 37, DateTimeZone.forID("UTC"))
      HttpDate.parse("Sun, 06 Nov 1994 08:49:37") must beSome(dt)
    }
  }
  "HttpDate.serialize" should {
    "serialize to 'Wed, 13 Jan 2021 22:23:01 UTC'" in {
      val dt = new DateTime(2021, 1, 13, 22, 23, 1, DateTimeZone.forID("GMT"))
      HttpDate.serialize(dt) must_== "Wed, 13 Jan 2021 22:23:01 UTC"
    }
    "serialize to 'Sun, 06 Nov 1994 08:49:37 UTC'" in {
      val dt = new DateTime(1994, 11, 6, 8, 49, 37, DateTimeZone.forID("GMT"))
      HttpDate.serialize(dt) must_== "Sun, 06 Nov 1994 08:49:37 UTC"
    }
    "serialize to 'Sun, 06 Nov 1994 08:49:37 UTC'" in {
      val dt = new DateTime(1994, 11, 6, 10, 49, 37, DateTimeZone.forID("+02:00"))
      HttpDate.serialize(dt) must_== "Sun, 06 Nov 1994 08:49:37 UTC"
    }
  }
}