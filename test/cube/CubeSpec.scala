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
  def age: Cube[Int]

  s"$name one-dimensional" should {
    "contain 3 values (values, sparse, dense)" in {
      age.values must have size 3
      age.sparse must have size 3
      age.dense must have size 3
    }
    "have get return the correct value for each year " ! {
      "year" || "age" |
        "2013" !! 32 |
        "2014" !! 33 |
        "2015" !! 34 |> { (y, a) ⇒
          val at = year.coordOf(y)
          age.get(at) must_== Some(a)
          age.isDefinedAt(at) must beTrue
          age(at) must_== a
        }
    }
    "have year as its one and only dimension" in {
      age.dimensions must_== Set(year)
    }
    "return itself as .raw" in {
      age.raw must_== age
    }
    "have an empty slice" in {
      age.slice must_== Point.empty
    }
  }
  s"$name one-dimensional sliced to 2014" should {
    def ageS = age.slice(Point(year.coordOf("2014")))
    "have slice Point(2014)" in {
      ageS.slice must_== Point(year.coordOf("2014"))
    }
    "have no dimension" in {
      ageS.dimensions must be empty
    }
    "contain one value (values, sparse, dense)" in {
      ageS.values must have size 1
      ageS.sparse must have size 1
      ageS.dense must have size 1
    }
    "return 33 for get(2014)" in {
      val at = year.coordOf("2014")
      ageS.get(at) must_== Some(33)
      ageS.isDefinedAt(at) must beTrue
      ageS(at) must_== 33
    }
    "be undefined at 2013, 2015" in {
      ageS.isDefinedAt(year.coordOf("2013")) must beFalse
      ageS.isDefinedAt(year.coordOf("2015")) must beFalse
    }
    "raw return the original cube" in {
      ageS.raw must_== age
    }
    "raw have 3 values" in {
      ageS.raw.values must have size 3
    }
  }
  s"$name one-dimensional diced to 2014" should {
    def ageD = age.dice(year, _ == year.coordOf("2014"))
    "have one dimension (year)" in {
      ageD.dimensions must_== Set(year)
    }
    "have empty slice" in {
      ageD.slice must_== Point.empty
    }
    "contain one value (values, sparse, dense)" in {
      ageD.values must have size 1
      ageD.sparse must have size 1
      ageD.dense must have size 1
    }
    "return 33 for get(2014)" in {
      val at = year.coordOf("2014")
      ageD.get(at) must_== Some(33)
      ageD.isDefinedAt(at) must beTrue
      ageD(at) must_== 33
    }
    "be undefined at 2013, 2015" in {
      ageD.isDefinedAt(year.coordOf("2013")) must beFalse
      ageD.isDefinedAt(year.coordOf("2015")) must beFalse
    }
    "raw return the original cube" in {
      ageD.raw must_== age
    }
    "raw have 3 values" in {
      ageD.raw.values must have size 3
    }
  }
}

class MapCubeSpec extends Specification {
  include(new CubeTck {
    val name = "MapCube"
    import Dimensions._
    val age = {
      val data = Map("2013" -> 32, "2014" -> 33, "2015" -> 34)
      MapCube(data.map(v ⇒ (Point(year.coordOf(v._1)), v._2)))
    }
  })
}