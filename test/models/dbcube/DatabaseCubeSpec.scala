package models.dbcube

import org.specs2.mutable._
import org.specs2.specification.BeforeAfterExample
import play.api.Play
import play.api.test._
import play.api.test.Helpers._
import cube._
import models._
import models.dbcube._
import Point._

class DatabaseCubeSpec extends Specification with BeforeAfterExample {
  include(new CubeTck("DatabaseCube") with BeforeAfterExample {

    override def before = Play.start(FakeApplication())
    override def after = Play.stop

    override def makeDimension(name: String, data: List[String]) = {
      val d = Dimension.create(name)
      data.foreach(d.add)
      d

    }
    override def makeAge(year: Dimension, data: Map[Point, Int]) = dbCubeForData(classOf[Int], data)
    override def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]) =
      dbCubeForData(classOf[Int], data)

    private def dbCubeForData[D](tpe: Class[D], data: Traversable[(Point, D)]) = {
      val cube = DatabaseCube.create(data.head._1.on, tpe)
      data.foreach { v ⇒
        val (point, value) = v
        cube.set(point, value)
      }
      cube
    }
  })

  def monat = Dimension.get("Monat").get
  def jan = monat.all(0)
  def feb = monat.all(1)
  def mar = monat.all(2)

  def oneDimensional = DatabaseCube.create(Set(monat), classOf[String])

  override def before = Play.start(FakeApplication())
  override def after = Play.stop

  "DatabaseCube with one dimension" should {
    "be droppable" in {
      val cube = oneDimensional
      DatabaseCube.delete(cube)
    }
    "be droppable if it has values" in {
      val cube = oneDimensional
      cube.set(jan, Some("1"))
      cube.set(feb, Some("2"))
      DatabaseCube.delete(cube)
    }
    "be initialized with all None" in {
      val cube = oneDimensional
      cube.dense.foreach(v ⇒ v._2 must beNone)
    }
    "return the set value" in {
      val cube = oneDimensional
      cube.set(jan, Some("1"))
      cube.get(jan) must equalTo(Some("1"))
    }
    "have all the value in all fields if set with setAll" in {
      val cube = oneDimensional
      cube.setAll(Some("X"))
      cube.dense.foreach(v ⇒ v._2 must beSome("X"))
      monat.all.foreach(p ⇒ cube.get(p) must beSome("X"))
    }
    "have sparse values only for set fields" in {
      val cube = oneDimensional
      cube.set(jan, Some("X"))
      cube.set(mar, Some("Y"))
      cube.values.toSet must equalTo(Set("X", "Y"))
      val exp: Set[(Point, String)] = Set((jan, "X"), (mar, "Y"))
      cube.sparse.toSet must equalTo(exp)
    }
    "be the same when loaded" in {
      val cube = oneDimensional
      cube.set(jan, Some("X"))
      val cube2 = DatabaseCube.load(cube.id, classOf[String]).get
      cube2.values.toSet must equalTo(Set("X"))
      cube2.get(jan) must beSome("X")
    }
  }
}