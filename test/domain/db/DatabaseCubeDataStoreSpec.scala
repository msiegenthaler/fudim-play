package domain.db

import org.specs2.mutable._
import org.specs2.specification.{Scope, BeforeAfterExample}
import base._
import cube._
import domain._
import models.playbinding._
import anorm.SqlParser._
import scala.Some
import support.withModel

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

  include(new CubeTck("DatabaseCube") with withModel with BeforeAfterExample with storeDataTypes {
    override def makeAge(year: Dimension, data: Map[Point, Int]) = execTx(dbCubeForData(intType, data))
    override def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]) =
      execTx(dbCubeForData(intType, data))

    override def makeDimension(name: String, data: List[String]) = {
      val d = super.makeDimension(name, data)
      dimensions = d :: dimensions
      d
    }
    private var dimensions: List[Dimension] = Nil
    private object CubeRepo extends DatabaseCubeDataStoreRepo with FudimResources {
      override def dimensionRepo = new DimensionRepository {
        def all = dimensions
      }
      override def storeTypes = storeDataTypes.all
      override def dataTypeRepo = dtr
    }
    private def dbCubeForData[T](dataType: DataType[T], data: Traversable[(Point, T)]) = {
      val cds = CubeRepo.create(data.head._1.on, dataType)
      val editor = cds.editor
      data.foreachTx {
        v ⇒
          val (point, value) = v
          editor.set(point, value)
      }
      cds.cube
    }
  })

  trait withplay extends withModel with storeDataTypes {
    var dimensions: List[Dimension] = Nil
    object CubeRepo extends DatabaseCubeDataStoreRepo with FudimResources {
      override def dimensionRepo = new DimensionRepository {
        def all = dimensions
      }
      override def storeTypes = storeDataTypes.all
      override def dataTypeRepo = dtr
    }
  }
  trait oneDimensionalCube extends withplay {
    lazy val monat = ListDimension("Monat", "Jan", "Feb", "Mar")
    def jan = monat.all(0)
    def feb = monat.all(1)
    def mar = monat.all(2)
    dimensions = monat :: dimensions
    lazy val cds = execTx(CubeRepo.create(Set(monat), stringType))
    def cube = cds.cube
    def editor = cds.editor
  }
  trait ort extends withplay {
    lazy val ort = ListDimension("Ort", "Bern", "NY", "Berlin")
    def bern = ort.all(0)
    def ny = ort.all(1)
    def berlin = ort.all(2)
    dimensions = ort :: dimensions
  }

  "DatabaseCube ListDimensionof type String with one dimension" should {
    "be createble" in new withplay {
      execTx {
        val d = ListDimension("TestDimension", "1", "2", "3")
        dimensions = d :: dimensions
        CubeRepo.create(Set(d), stringType)
      }
    }
    "be loadable" in new oneDimensionalCube {
      CubeRepo.get(cds.id) must beSome
    }
    "be droppable" in new oneDimensionalCube {
      execTx {
        CubeRepo.remove(cds.id)
        CubeRepo.get(cds.id) must beNone
      }
    }
    "be droppable if it has values" in new oneDimensionalCube {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))
        CubeRepo.remove(cds.id)
      }
    }
    "be initialized with all None" in new oneDimensionalCube {
      cube.dense.foreach(v ⇒ v._2 must beNone)
    }
    "return the set value" in new oneDimensionalCube {
      execTx {
        editor.set(jan, Some("1"))
        cube.get(jan) must equalTo(Some("1"))
      }
    }
    "have all the value in all fields if set with setAll" in new oneDimensionalCube {
      execTx {
        editor.multiSet(Point.empty, Some("X"))
        cube.dense.foreach(v ⇒ v._2 must beSome("X"))
        monat.all.foreach(p ⇒ cube.get(p) must beSome("X"))
      }
    }
    "have sparse values only for set fields" in new oneDimensionalCube {
      execTx {
        editor.set(jan, Some("X"))
        editor.set(mar, Some("Y"))
        cube.values.toSet must equalTo(Set("X", "Y"))
        val exp: Set[(Point, String)] = Set((jan, "X"), (mar, "Y"))
        cube.sparse.toSet must equalTo(exp)
      }
    }
    "be the same when loaded" in new oneDimensionalCube {
      execTx {
        editor.set(jan, Some("X"))
        val cds2 = CubeRepo.get(cds.id, stringType).get
        cds2.cube.values.toSet must equalTo(Set("X"))
        cds2.cube.get(jan) must beSome("X")
      }
    }

    "serialize to json" in new oneDimensionalCube {
      val ser = CubeRepo.json.serializer
      ser.isDefinedAt(cds) must beTrue
      ser(cds).isSuccess must beTrue
    }
    "reparse from json" in new oneDimensionalCube {
      execTx {
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

    "extendable to Ort-dimension (values moved to Bern)" in new oneDimensionalCube with ort {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))

        val cds2 = cds.copy(bern)
        cds2.dataType must_== cds.dataType
        cds2.cube.get(jan + bern) must beSome("1")
        cds2.cube.get(feb + bern) must beSome("2")
        cds2.cube.get(mar + bern) must beNone
        cds2.cube.get(jan + ny) must beNone
        cds2.cube.sparse.size must_== 2
        cds.cube.get(jan) must beSome("1")
      }
    }
    "extendable to Ort-dimension (values moved to NY)" in new oneDimensionalCube with ort {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))

        val cds2 = cds.copy(ny)
        cds2.dataType must_== cds.dataType
        cds2.cube.get(jan + ny) must beSome("1")
        cds2.cube.get(feb + ny) must beSome("2")
        cds2.cube.get(mar + ny) must beNone
        cds2.cube.get(jan + bern) must beNone
        cds2.cube.sparse.size must_== 2
        cds.cube.get(jan) must beSome("1")
      }
    }
    "rereducable to one dimension" in new oneDimensionalCube with ort {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))
        val cds2 = cds.copy(bern)
        cds2.editor.set(ny + mar, Some("Test"))

        val cds3 = cds2.copy(Point.empty, bern)
        cds3.cube.get(jan) must beSome("1")
        cds3.cube.get(feb) must beSome("2")
        cds3.cube.get(mar) must beNone
        cds2.cube.get(jan + bern) must beSome("1")
        cds2.cube.get(feb + bern) must beSome("2")
        cds2.cube.get(mar + bern) must beNone
        cds2.cube.get(mar + ny) must beSome("Test")
      }
    }
    "rereducable to one dimension and take not first coordinate" in new oneDimensionalCube with ort {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))
        val cds2 = cds.copy(bern)
        cds2.editor.set(ny + mar, Some("Test"))

        val cds3 = cds2.copy(Point.empty, ny)
        cds3.cube.get(jan) must beNone
        cds3.cube.get(feb) must beNone
        cds3.cube.get(mar) must beSome("Test")
        cds2.cube.get(jan + bern) must beSome("1")
        cds2.cube.get(feb + bern) must beSome("2")
        cds2.cube.get(mar + bern) must beNone
        cds2.cube.get(mar + ny) must beSome("Test")
      }
    }
    "rereducable to one dimension and take not first coordinate when values overlap" in new oneDimensionalCube with ort {
      execTx {
        editor.set(jan, Some("1"))
        editor.set(feb, Some("2"))
        editor.set(mar, Some("3"))
        val cds2 = cds.copy(bern)
        cds2.editor.set(ny + jan, Some("Bla"))
        cds2.editor.set(ny + mar, Some("Test"))

        val cds3 = cds2.copy(Point.empty, ny)
        cds3.cube.get(jan) must beSome("Bla")
        cds3.cube.get(feb) must beNone
        cds3.cube.get(mar) must beSome("Test")
        cds2.cube.get(jan + bern) must beSome("1")
        cds2.cube.get(feb + bern) must beSome("2")
        cds2.cube.get(mar + bern) must beSome("3")
        cds2.cube.get(mar + ny) must beSome("Test")
      }
    }
  }
}
