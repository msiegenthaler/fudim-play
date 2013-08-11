package cube

import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables

abstract class CubeTck(name: String) extends Specification with DataTables {
  object Dimensions {
    val year = ListDimension("year", "2013", "2014", "2015")
    val color = ListDimension("color", "red", "green", "white")
    val location = ListDimension("location", "Bern", "NY", "Shanghai")
    val product = ListDimension("product", "shirt", "socks")
  }
  import Dimensions._

  /**
   * One-Dimensional cube containing the fact "Mario's age" over the year-Dimension.
   * Data: (2013 -> 32, 2014 -> 33, 2015 -> 34)
   */
  def age: Cube[Int]
  val ageData = Map("2013" -> 32, "2014" -> 33, "2015" -> 34).map(v ⇒ (Point(year.coordOf(v._1)), v._2))

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
    "as raw return the original cube" in {
      ageD.raw must_== age
    }
    ".raw have 3 values" in {
      ageD.raw.values must have size 3
    }
  }

  /**
   * Three dimensional cube containing the number of sold units for color, location and product.
   * Data:
   * 	shirt/Bern: red -> 20, green -> 15, white -> 100
   *  shirt/NY: red -> 10, green -> 5, white -> 2
   *  shirt/Shanghai: red -> 0, green -> 1, white -> 0
   *  socks/Bern: red -> 100, green -> 120, white -> 555
   *  socks/NY: red -> 200, green -> 10, white -> 1500
   *  socks/Shanghai: no values
   */
  def sales: Cube[Int]
  val salesData = {
    val d = ("shirt", "Bern", 20, 15, 100) :: ("shirt", "NY", 10, 5, 2) :: ("shirt", "Shanghai", 0, 1, 0) ::
      ("socks", "Bern", 100, 120, 555) :: ("socks", "NY", 200, 10, 1500) :: Nil
    d.flatMap { v ⇒
      (Point(product.coordOf(v._1), location.coordOf(v._2), color.coordOf("red")), v._3) ::
        (Point(product.coordOf(v._1), location.coordOf(v._2), color.coordOf("green")), v._4) ::
        (Point(product.coordOf(v._1), location.coordOf(v._2), color.coordOf("white")), v._5) :: Nil
    }
  }.toMap

  s"$name sales (three-dimensional)" should {
    def p(p: String, l: String, c: String) = Point(product.coordOf(p), location.coordOf(l), color.coordOf(c))
    "contain 15 values" in {
      sales.values must have size 15
      sales.sparse must have size 15
    }
    "contain 18 points" in {
      sales.dense must have size 18
    }
    "contain 15 defined points" in {
      sales.dense.filter(_._2.isDefined) must have size 15
    }
    "contain 3 empty points" in {
      sales.dense.filter(_._2.isEmpty) must have size 3
    }
    "have a total of 2638 products sold (when aggregating values)" in {
      sales.values.reduce(_ + _) must_== 2638
    }
    "have a total of 2638 products sold (when aggregating sparse)" in {
      sales.sparse.map(_._2).reduce(_ + _) must_== 2638
    }
    "have a total of 2638 products sold (when aggregating dense)" in {
      sales.dense.map(_._2).flatten.reduce(_ + _) must_== 2638
    }
    "have sold 555 white socks in Bern" in {
      val at = p("socks", "Bern", "white")
      sales.get(at) must beSome(555)
      sales.isDefinedAt(at) must beTrue
      sales(at) must_== 555
    }
    "have sold 5 green shirts in NY" in {
      val at = p("shirt", "NY", "green")
      sales.get(at) must beSome(5)
      sales.isDefinedAt(at) must beTrue
      sales(at) must_== 5
    }
    "have no data for white sock sales in shanghai" in {
      sales.get(p("socks", "Shanghai", "white")) must beNone
    }
    "have no aggregated data in products" in {
      sales.get(product.all.head) must beNone
      sales.isDefinedAt(product.all.head) must beFalse
    }
    "have no aggregated data for Bern/shirt" in {
      sales.get(Point(product.coordOf("shirt"), location.coordOf("Bern"))) must beNone
    }
    "have product, location and color as dimension" in {
      sales.dimensions must_== Set(product, location, color)
    }
    "as slice return empty" in {
      sales.slice must_== Point.empty
    }
    "as raw return itself" in {
      sales.raw must_== sales
    }
  }
  s"$name sales (three-dimensional) sliced to Bern" should {
    def p(p: String, l: String, c: String) = Point(product.coordOf(p), location.coordOf(l), color.coordOf(c))
    def sales2 = sales.slice(location.coordOf("Bern"))
    "as raw return original cube" in {
      sales2.raw must_== sales
    }
    "as slice return Point(Bern)" in {
      sales2.slice must_== Point(location.coordOf("Bern"))
    }
    "have the dimensions product and color" in {
      sales2.dimensions must_== Set(product, color)
    }
    "contain 6 values" in {
      sales2.values must have size 6
      sales2.sparse must have size 6
    }
    "contain 6 defined values" in {
      sales2.dense must have size 6
      sales2.dense.filter(_._2.isDefined) must have size 6
    }
    "have a total of 910 products sold" in {
      sales2.values.reduce(_ + _) must_== 910
      sales2.sparse.map(_._2).reduce(_ + _) must_== 910
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 910
    }
    "be undefined for green shirts in NY" in {
      val at = p("shirt", "NY", "green")
      sales2.get(at) must beNone
      sales2.isDefinedAt(at) must beFalse
    }
    "have no aggregated data for Bern/shirt" in {
      sales2.get(Point(product.coordOf("shirt"), location.coordOf("Bern"))) must beNone
    }
    "have sold 555 white socks in Bern" in {
      val at = p("socks", "Bern", "white")
      sales2.get(at) must beSome(555)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 555
    }
  }
  s"$name sales (three-dimensional) sliced to Bern/socks" should {
    def p(p: String, l: String, c: String) = Point(product.coordOf(p), location.coordOf(l), color.coordOf(c))
    val sp = Point(location.coordOf("Bern"), product.coordOf("socks"))
    def sales2 = sales.slice(sp)
    "as raw return original cube" in {
      sales2.raw must_== sales
    }
    "as slice return Point(Bern, socks)" in {
      sales2.slice must_== sp
    }
    "have the dimension color" in {
      sales2.dimensions must_== Set(color)
    }
    "contain 3 values" in {
      sales2.values must have size 3
      sales2.sparse must have size 3
    }
    "contain 3 defined values" in {
      sales2.dense must have size 3
      sales2.dense.filter(_._2.isDefined) must have size 3
    }
    "have a total of 775 products sold" in {
      sales2.values.reduce(_ + _) must_== 775
      sales2.sparse.map(_._2).reduce(_ + _) must_== 775
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 775
    }
    "be undefined for green shirts in NY" in {
      val at = p("shirt", "NY", "green")
      sales2.get(at) must beNone
      sales2.isDefinedAt(at) must beFalse
    }
    "have no aggregated data for Bern/socks" in {
      sales2.get(Point(product.coordOf("socks"), location.coordOf("Bern"))) must beNone
    }
    "have sold 555 white socks in Bern" in {
      val at = p("socks", "Bern", "white")
      sales2.get(at) must beSome(555)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 555
    }
  }
  s"$name sales (three-dimensional) sliced to green socks" should {
    def p(p: String, l: String, c: String) = Point(product.coordOf(p), location.coordOf(l), color.coordOf(c))
    val sp = Point(color.coordOf("green"), product.coordOf("socks"))
    def sales2 = sales.slice(sp)
    "as raw return original cube" in {
      sales2.raw must_== sales
    }
    "as slice return Point(green, socks)" in {
      sales2.slice must_== sp
    }
    "have the dimension location" in {
      sales2.dimensions must_== Set(location)
    }
    "contain 2 values" in {
      sales2.values must have size 2
      sales2.sparse must have size 2
    }
    "contain 2 defined values" in {
      sales2.dense.filter(_._2.isDefined) must have size 2
    }
    "contain 3 points" in {
      sales2.dense must have size 3
    }
    "have a total of 130 products sold" in {
      sales2.values.reduce(_ + _) must_== 130
      sales2.sparse.map(_._2).reduce(_ + _) must_== 130
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 130
    }
    "have sold 120 green socks in Bern" in {
      val at = p("socks", "Bern", "green")
      sales2.get(at) must beSome(120)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 120
    }
  }
  s"$name sales (three-dimensional) diced to green and red" should {
    def sales2 = sales.dice(color, c ⇒ color.render(c) == "green" || color.render(c) == "red")
    "as raw return original cube" in {
      sales2.raw must_== sales
    }
    "as slice return Point.empty" in {
      sales2.slice must_== Point.empty
    }
    "have product, location and color as dimension" in {
      sales2.dimensions must_== Set(product, location, color)
    }
    "contain 10 values" in {
      sales2.values must have size 10
      sales2.sparse must have size 10
      sales2.dense.filter(_._2.isDefined) must have size 10
    }
    "contain 12 points" in {
      sales2.dense must have size 12
    }
    "have a total of 481 products sold" in {
      sales2.values.reduce(_ + _) must_== 481
      sales2.sparse.map(_._2).reduce(_ + _) must_== 481
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 481
    }
  }

  s"$name sales (three-dimensional) sliced to socks and diced to green and red" should {
    def sales2 = sales.
      dice(color, c ⇒ color.render(c) == "green" || color.render(c) == "red").
      slice(product.coordOf("socks"))
    "as raw return original cube" in {
      sales2.raw must_== sales
    }
    "as slice return socks" in {
      sales2.slice must_== Point(product.coordOf("socks"))
    }
    "have location and color as dimension" in {
      sales2.dimensions must_== Set(location, color)
    }
    "contain 4 values" in {
      sales2.values must have size 4
      sales2.sparse must have size 4
      sales2.dense.filter(_._2.isDefined) must have size 4
    }
    "contain 6 points" in {
      sales2.dense must have size 6
    }
    "have a total of 430 products sold" in {
      sales2.values.reduce(_ + _) must_== 430
      sales2.sparse.map(_._2).reduce(_ + _) must_== 430
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 430
    }
  }
}

class MapCubeSpec extends Specification {
  include(new CubeTck("MapCube") {
    val age = MapCube(ageData)
    val sales = MapCube(salesData)
  })
}