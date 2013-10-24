package support.http

import org.specs2.mutable.Specification

class CacheHeadersSpec extends Specification {
  "EntityTag" should {
    "should have quoted value for strong" in {
      EntityTag("1a2b3c").value must_== """"1a2b3c""""
    }
    "have a value prefixed with W/ for weak" in {
      EntityTag("1a2b3c", isWeak = true).value must_== """W/"1a2b3c""""
    }
  }
  "EntityTag.parse" should {
    """parse a strong etag from "a"""" in {
      val etag = EntityTag.parse(""""a"""").get
      etag.isWeak should beFalse
      etag.id must_== "a"
    }
    """parse a strong etag from "123456789"""" in {
      val etag = EntityTag.parse(""""123456789"""").get
      etag.isWeak should beFalse
      etag.id must_== "123456789"
    }
    """parse a weak etag from W/"123456789"""" in {
      val etag = EntityTag.parse("""W/"123456789"""").get
      etag.isWeak should beTrue
      etag.id must_== "123456789"
    }
    "not parse unquoted string" in {
      EntityTag.parse("bla") must beNone
      EntityTag.parse("W/bla") must beNone
    }
    "not parse an unquoted number" in {
      EntityTag.parse("2134") must beNone
      EntityTag.parse("W/2134") must beNone
    }
    "not parse an unclosed quote" in {
      EntityTag.parse("\"2134") must beNone
      EntityTag.parse("W/\"2134") must beNone
    }
    "not parse an unknown prefix" in {
      EntityTag.parse("A/\"2134\"") must beNone
    }
  }
  "EntityTag.hashed" should {
    "create etag for a string that is a 32-character hex" in {
      val v = "some funny string that is very cool"
      val etag = EntityTag.hashed(v)
      etag.isWeak must beFalse
      etag.id.length must_== 32
      etag.id.matches("[0-9a-f]{32}") must beTrue
    }
    "create etag for a string that is stable" in {
      val v = "some funny string that is very cool"
      val etag = EntityTag.hashed(v)
      etag must_== EntityTag.hashed(v)
    }
    "create etag for a string that is different for other values" in {
      val v = "some funny string that is very cool"
      val etag = EntityTag.hashed(v)
      etag must_!= EntityTag.hashed("hello")
    }
    "create a parsable etag" in {
      val v = "some funny string that is very cool"
      val etag = EntityTag.hashed(v)
      EntityTag.parse(etag.value) must beSome(etag)
    }
  }

  "CacheHeaders.ifNoneMatch" should {
    "handle a * value" in pending
    "handle a single value" in pending
    "handle multiple values separated by spaces" in pending
  }
}