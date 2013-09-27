package models.dbcube

import java.sql.Connection
import org.specs2.mutable._
import org.specs2.specification.{ Scope, BeforeAfterExample }
import play.api.Play
import play.api.Play.current
import play.api.db.DB
import play.api.test._
import cube._
import domain._
import models.FudimDataTypes
import anorm.SqlParser._
import scala.Some
import play.api.test.FakeApplication

class DatabaseCubeDataStoreSpec extends Specification {
  trait storeDataTypes extends domain.TestFixtures.dataTypes {
    object storeDataTypes {
      object string extends StoreDataType[String] {
        val dataType = stringType
        val sqlType = "varchar(1024)"
        def fromDb(name: String) = str(name)
        def toDb(value: String) = value
      }
      object integer extends StoreDataType[Int] {
        val dataType = intType
        val sqlType = "integer"
        def fromDb(name: String) = int(name)
        def toDb(value: Int) = value
      }

      val all = string :: integer :: Nil
    }
    def dtr = dataTypeRepo
  }

  include(new CubeTck("DatabaseCube") with BeforeAfterExample with storeDataTypes {
    override def before = Play.start(FakeApplication())
    override def after = Play.stop

    override def makeAge(year: Dimension, data: Map[Point, Int]) = dbCubeForData(intType, data)
    override def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]) =
      dbCubeForData(intType, data)

    override def makeDimension(name: String, data: List[String]) = {
      val d = super.makeDimension(name, data)
      dimensions = d :: dimensions
      d
    }
    private var dimensions: List[Dimension] = Nil
    private object CubeRepo extends DatabaseCubeDataStoreRepo {
      override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
      override def dimensionRepo = new DimensionRepository {
        def all = dimensions
      }
      override def storeTypes = storeDataTypes.all
      override def dataTypeRepo = dtr
    }
    private def dbCubeForData[T](dataType: DataType[T], data: Traversable[(Point, T)]) = {
      val cds = CubeRepo.create(data.head._1.on, dataType)
      val editor = cds.editor
      data.foreach { v ⇒
        val (point, value) = v
        editor.set(point, value)
      }
      cds.cube
    }
  })

  trait withplay extends Scope with BeforeAfter with storeDataTypes {
    override def before = Play.start(FakeApplication())
    override def after = Play.stop

    var dimensions: List[Dimension] = Nil
    object CubeRepo extends DatabaseCubeDataStoreRepo {
      override def withConnection[A](f: Connection ⇒ A) = DB.withConnection(f)
      override def dimensionRepo = new DimensionRepository {
        def all = dimensions
      }
      override def storeTypes = storeDataTypes.all
      override def dataTypeRepo = dtr
    }
  }
  trait oneDimensionalCube extends withplay  {
    lazy val monat = ListDimension("Monat", "Jan", "Feb", "Mar")
    def jan = monat.all(0)
    def feb = monat.all(1)
    def mar = monat.all(2)
    dimensions = monat :: dimensions
    lazy val cds = CubeRepo.create(Set(monat), stringType)
    def cube = cds.cube
    def editor = cds.editor
  }

  "DatabaseCube ListDimensionof type String with one dimension" should {
    "be createble" in new withplay {
      val d = ListDimension("TestDimension", "1", "2", "3")
      dimensions = d :: dimensions
      CubeRepo.create(Set(d), stringType)
    }
    "be loadable" in new oneDimensionalCube {
      CubeRepo.load(cds.id) must beSome
    }
    "be droppable" in new oneDimensionalCube {
      CubeRepo.delete(cds)
      CubeRepo.load(cds.id) must beNone
    }
    "be droppable if it has values" in new oneDimensionalCube {
      editor.set(jan, Some("1"))
      editor.set(feb, Some("2"))
      CubeRepo.delete(cds)
    }
    "be initialized with all None" in new oneDimensionalCube {
      cube.dense.foreach(v ⇒ v._2 must beNone)
    }
    "return the set value" in new oneDimensionalCube {
      editor.set(jan, Some("1"))
      cube.get(jan) must equalTo(Some("1"))
    }
    "have all the value in all fields if set with setAll" in new oneDimensionalCube {
      editor.multiSet(Point.empty, Some("X"))
      cube.dense.foreach(v ⇒ v._2 must beSome("X"))
      monat.all.foreach(p ⇒ cube.get(p) must beSome("X"))
    }
    "have sparse values only for set fields" in new oneDimensionalCube {
      editor.set(jan, Some("X"))
      editor.set(mar, Some("Y"))
      cube.values.toSet must equalTo(Set("X", "Y"))
      val exp: Set[(Point, String)] = Set((jan, "X"), (mar, "Y"))
      cube.sparse.toSet must equalTo(exp)
    }
    "be the same when loaded" in new oneDimensionalCube {
      editor.set(jan, Some("X"))
      val cds2 = CubeRepo.load(cds.id, stringType).get
      cds2.cube.values.toSet must equalTo(Set("X"))
      cds2.cube.get(jan) must beSome("X")
    }

    "serialize to json" in new oneDimensionalCube {
      val ser = CubeRepo.json.serializer
      ser.isDefinedAt(cds) must beTrue
      ser(cds).isSuccess must beTrue
    }
    "reparse from json" in new oneDimensionalCube {
      editor.set(jan, Some("X"))
      val p = for {
        json ← CubeRepo.json.serializer(cds)
        c2 ← CubeRepo.json.parser(json)
      } yield c2
      p.isSuccess must beTrue

      val cds2 = p.toOption.get
      cds2.id must_== cds.id
      cds2.cube.get(jan) must beSome("X")
    }
  }
}
