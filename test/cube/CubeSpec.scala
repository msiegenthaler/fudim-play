package cube

import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables

trait CubeTck extends Specification with DataTables {
  val name: String

  object Dimensions {
    val year = ListDimension("year", "2013", "2014", "2015")
  }
  import Dimensions._

  /**
   * One-Dimensional cube containing the fact "Mario's age" over the year-Dimension.
   * Data: (2013 -> 32, 2014 -> 33, 2015 -> 34)
   */
  def oneD: Cube[Int]

  s"$name one-dimensional" should {
    "contain 3 values (values, sparse, dense)" in {
      oneD.values must have size 3
      oneD.sparse must have size 3
      oneD.dense must have size 3
    }
    "have get return the correct value for each year " ! {
      "year" || "age" |
        "2013" !! 32 |
        "2014" !! 33 |
        "2015" !! 34 |> { (y, age) ⇒
          val at = year.coordOf(y)
          oneD.get(at) must_== Some(age)
          oneD.isDefinedAt(at) must beTrue
          oneD(at) must_== age
        }
    }
    "have year as its one and only dimension" in {
      oneD.dimensions must_== Set(year)
    }
    "return itself as .raw" in {
      oneD.raw must_== oneD
    }
    "have an empty slice" in {
      oneD.slice must_== Point.empty
    }
  }
  s"$name one-dimensional sliced to 2014" should {
    def oneDS = oneD.slice(Point(year.coordOf("2014")))
    "have slice Point(2014)" in {
      oneDS.slice must_== Point(year.coordOf("2014"))
    }
    "have no dimension" in {
      oneDS.dimensions must be empty
    }
    "contain one value (values, sparse, dense)" in {
      oneDS.values must have size 1
      oneDS.sparse must have size 1
      oneDS.dense must have size 1
    }
    "return 33 for get(2014)" in {
      val at = year.coordOf("2014")
      oneDS.get(at) must_== Some(33)
      oneDS.isDefinedAt(at) must beTrue
      oneDS(at) must_== 33
    }
    "be undefined at 2013, 2015" in {
      oneDS.isDefinedAt(year.coordOf("2013")) must beFalse
      oneDS.isDefinedAt(year.coordOf("2015")) must beFalse
    }
    "raw return the original cube" in {
      oneDS.raw must_== oneD
    }
    "raw have 3 values" in {
      oneDS.raw.values must have size 3
    }
  }
  s"$name one-dimensional diced to 2014" should {
    def oneDD = oneD.dice(year, _ == year.coordOf("2014"))
    "have one dimension (year)" in {
      oneDD.dimensions must_== Set(year)
    }
    "have empty slice" in {
      oneDD.slice must_== Point.empty
    }
    "contain one value (values, sparse, dense)" in {
      oneDD.values must have size 1
      oneDD.sparse must have size 1
      oneDD.dense must have size 1
    }
    "return 33 for get(2014)" in {
      val at = year.coordOf("2014")
      oneDD.get(at) must_== Some(33)
      oneDD.isDefinedAt(at) must beTrue
      oneDD(at) must_== 33
    }
    "be undefined at 2013, 2015" in {
      oneDD.isDefinedAt(year.coordOf("2013")) must beFalse
      oneDD.isDefinedAt(year.coordOf("2015")) must beFalse
    }
    "raw return the original cube" in {
      oneDD.raw must_== oneD
    }
    "raw have 3 values" in {
      oneDD.raw.values must have size 3
    }
  }
}

class MapCubeSpec extends Specification {
  include(new CubeTck {
    val name = "MapCube"
    import Dimensions._
    val oneD = {
      val data = Map("2013" -> 32, "2014" -> 33, "2015" -> 34)
      MapCube(data.map(v ⇒ (Point(year.coordOf(v._1)), v._2)))
    }
  })
}