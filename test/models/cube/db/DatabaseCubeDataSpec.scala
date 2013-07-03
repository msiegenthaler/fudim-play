package test.models.cube.db

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import models._
import models.cube._
import models.cube.db._
import Point._

class DatabaseCubeDataSpec extends Specification {
  def monat = Dimension.get("Monat").get
  def jan = monat.all(0)
  def feb = monat.all(1)
  def mar = monat.all(2)

  def oneDimensional = DatabaseCubeData.create(Set(monat), classOf[String])

  "DatabaseCubeData with one dimension" should {
    "be droppable" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        DatabaseCubeData.delete(cube)
      }
    }
    "be droppable if it has values" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.set(jan, Some("1"))
        cube.set(feb, Some("2"))
        DatabaseCubeData.delete(cube)
      }
    }
    "be initialized with all None" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.dense.foreach(v ⇒ v._2 must beNone)
      }
    }
    "return the set value" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.set(jan, Some("1"))
        cube.get(jan) must equalTo(Some("1"))
      }
    }
    "have all the value in all fields if set with setAll" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.setAll(Some("X"))
        cube.dense.foreach(v ⇒ v._2 must beSome("X"))
        monat.all.foreach(p ⇒ cube.get(p) must beSome("X"))
      }
    }
    "have sparse values only for set fields" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.set(jan, Some("X"))
        cube.set(mar, Some("Y"))
        cube.values.toSet must equalTo(Set("X", "Y"))
        val exp: Set[(Point, String)] = Set((jan, "X"), (mar, "Y"))
        cube.sparse.toSet must equalTo(exp)
      }
    }
    "be the same when loaded" in {
      running(FakeApplication()) {
        val cube = oneDimensional
        cube.set(jan, Some("X"))
        val cube2 = DatabaseCubeData.load(cube.id, classOf[String]).get
        cube2.values.toSet must equalTo(Set("X"))
        cube2.get(jan) must beSome("X")
      }
    }
  }
}