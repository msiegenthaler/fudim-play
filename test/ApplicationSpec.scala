package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

class ApplicationSpec extends Specification {
  "Application" should {
    "boot" in {
      running(FakeApplication()) {
        val result = route(FakeRequest(GET, "/")).get
        status(result) must equalTo(303)
      }
    }
    "send 404 on a bad request" in {
      running(FakeApplication()) {
        route(FakeRequest(GET, "/does-not-exist")) must beNone
      }
    }
  }
}
