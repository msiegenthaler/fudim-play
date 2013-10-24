package support.http

import org.specs2.mutable.Specification
import play.api.mvc.Headers

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

  def h(vs: (String, String)*): Headers = {
    new Headers {
      override val data = vs.groupBy(_._1).mapValues(_.map(_._2)).toSeq
    }
  }

  def hNoneMatch(v: String*) = h(v.map(value ⇒ "If-None-Match" -> value): _*)

  "CacheHeaders.ifNoneMatch" should {
    "handle a * value" in {
      hNoneMatch("*").ifNoneMatch must beLeft(AllETags)
    }
    "handle a single value" in {
      hNoneMatch("\"1d2c1a\"").ifNoneMatch must beRight(Set(EntityTag("1d2c1a")))
    }
    "handle a single weak value" in {
      hNoneMatch("W/\"1d2c1a\"").ifNoneMatch must beRight(Set(EntityTag("1d2c1a", true)))
    }
    "handle multiple values in multiple header repeats" in {
      val h = hNoneMatch("\"a\"", "\"b\"", "\"c\"")
      h.ifNoneMatch must beRight(Set(EntityTag("a"), EntityTag("b"), EntityTag("c")))
    }
    "handle multiple values separated by colons" in {
      val h = hNoneMatch(""""a","b","c"""")
      h.ifNoneMatch must beRight(Set(EntityTag("a"), EntityTag("b"), EntityTag("c")))
    }
    "handle multiple values separated by colons and spaces" in {
      val h = hNoneMatch(""""a", "b", "c"""")
      h.ifNoneMatch must beRight(Set(EntityTag("a"), EntityTag("b"), EntityTag("c")))
    }
    "handle multiple weak values separated by colons" in {
      val h = hNoneMatch("""W/"a","b",W/"c"""")
      h.ifNoneMatch must beRight(Set(EntityTag("a", true), EntityTag("b"), EntityTag("c", true)))
    }
  }
}