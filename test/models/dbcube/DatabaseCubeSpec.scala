package models.dbcube

import java.sql.Connection
import org.specs2.mutable._
import org.specs2.specification.{ Scope, BeforeAfterExample }
import play.api.Play
import play.api.Play.current
import play.api.db.DB
import play.api.test._
import play.api.test.Helpers._
import cube._
import models._
import models.dbcube._
import models.db.DatabaseDimensionRepo

class DatabaseCubeSpec extends Specification {
  include(new CubeTck("DatabaseCube") with BeforeAfterExample {
    override def before = Play.start(FakeApplication())
    override def after = Play.stop

    override def makeAge(year: Dimension, data: Map[Point, Int]) = dbCubeForData(classOf[Int], data)
    override def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]) =
      dbCubeForData(classOf[Int], data)

    override def makeDimension(name: String, data: List[String]) = {
      val d = super.makeDimension(name, data)
      dimensions = d :: dimensions
      d
    }
    private var dimensions: List[Dimension] = Nil
    private object CubeRepo extends DatabaseCubeRepo {
      override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
      override def dimension(name: String) = dimensions.find(_.name == name)
    }
    private def dbCubeForData[T](tpe: Class[T], data: Traversable[(Point, T)]) = {
      val cube = CubeRepo.create(data.head._1.on, tpe)
      data.foreach { v ⇒
        val (point, value) = v
        cube.set(point, value)
      }
      cube
    }
  })

  trait withplay extends Scope with BeforeAfter {
    override def before = Play.start(FakeApplication())
    override def after = Play.stop

    var dimensions: List[Dimension] = Nil
    object CubeRepo extends DatabaseCubeRepo {
      override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
      override def dimension(name: String) = dimensions.find(_.name == name)
    }
  }
  trait oneDimensionalCube extends withplay {
    lazy val monat = ListDimension("Monat", "Jan", "Feb", "Mar")
    def jan = monat.all(0)
    def feb = monat.all(1)
    def mar = monat.all(2)
    dimensions = monat :: dimensions
    lazy val cube = CubeRepo.create(Set(monat), classOf[String])
  }

  "DatabaseCube ListDimensionof type String with one dimension" should {
    "be createble" in new withplay {
      val d = ListDimension("TestDimension", "1", "2", "3")
      dimensions = d :: dimensions
      CubeRepo.create(Set(d), classOf[String])
    }
    "be loadable" in new oneDimensionalCube {
      CubeRepo.load(cube.id) must beSome
    }
    "be droppable" in new oneDimensionalCube {
      CubeRepo.delete(cube)
      CubeRepo.load(cube.id) must beNone
    }
    "be droppable if it has values" in new oneDimensionalCube {
      cube.set(jan, Some("1"))
      cube.set(feb, Some("2"))
      CubeRepo.delete(cube)
    }
    "be initialized with all None" in new oneDimensionalCube {
      cube.dense.foreach(v ⇒ v._2 must beNone)
    }
    "return the set value" in new oneDimensionalCube {
      cube.set(jan, Some("1"))
      cube.get(jan) must equalTo(Some("1"))
    }
    "have all the value in all fields if set with setAll" in new oneDimensionalCube {
      cube.setAll(Some("X"))
      cube.dense.foreach(v ⇒ v._2 must beSome("X"))
      monat.all.foreach(p ⇒ cube.get(p) must beSome("X"))
    }
    "have sparse values only for set fields" in new oneDimensionalCube {
      cube.set(jan, Some("X"))
      cube.set(mar, Some("Y"))
      cube.values.toSet must equalTo(Set("X", "Y"))
      val exp: Set[(Point, String)] = Set((jan, "X"), (mar, "Y"))
      cube.sparse.toSet must equalTo(exp)
    }
    "be the same when loaded" in new oneDimensionalCube {
      cube.set(jan, Some("X"))
      val cube2 = CubeRepo.load(cube.id, classOf[String]).get
      cube2.values.toSet must equalTo(Set("X"))
      cube2.get(jan) must beSome("X")
    }

    "serialize to json" in new oneDimensionalCube {
      val ser = CubeRepo.json.serializer
      ser.isDefinedAt(cube) must beTrue
      ser(cube).isSuccess must beTrue
    }
    "reparse from json" in new oneDimensionalCube {
      cube.set(jan, Some("X"))
      val p = for {
        json ← CubeRepo.json.serializer(cube)
        c2 ← CubeRepo.json.parser(json)
      } yield c2
      p.isSuccess must beTrue

      val cube2 = p.toOption.get
      cube2.id must_== cube.id
      cube2.get(jan) must beSome("X")
    }
  }
}