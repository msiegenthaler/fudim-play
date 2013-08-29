package cube

import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables
import org.specs2.specification.Scope

abstract class CubeTck(name: String) extends Specification with DataTables {
  def makeDimension(name: String, data: List[String]): Dimension
  def makeAge(year: Dimension, data: Map[Point, Int]): Cube[Int]
  def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]): Cube[Int]

  def coordOf(d: Dimension, v: String): Coordinate = {
    d.values.find(_._2 == v).
      getOrElse(throw new IllegalArgumentException(s"value $v is not a year. Valid values are ${d.values.map(_._2)}")).
      _1
  }

  trait year extends Scope {
    private def yearValues = "2013" :: "2014" :: "2015" :: Nil
    val year = makeDimension("year", yearValues)
    def year(v: String): Coordinate = coordOf(year, v)
  }
  trait age extends year {
    private def ageData = Map("2013" -> 32, "2014" -> 33, "2015" -> 34).map(v ⇒ (Point(year(v._1)), v._2))
    /** One-Dimensional cube containing the fact "Mario's age" over the year-Dimension. */
    val age = makeAge(year, ageData)
  }

  s"$name one-dimensional" should {
    "contain 3 values (values, sparse, dense)" in new age {
      age.values must have size 3
      age.sparse must have size 3
      age.dense must have size 3
    }
    "have get return the correct value for each year " ! new age {
      "year" || "age" |
        "2013" !! 32 |
        "2014" !! 33 |
        "2015" !! 34 |> { (y, a) ⇒
          val at = year(y)
          age.get(at) must_== Some(a)
          age.isDefinedAt(at) must beTrue
          age(at) must_== a
        }
    }
    "have year as its one and only dimension" in new age {
      age.dimensions must_== Set(year)
    }
    "return itself as .raw" in new age {
      age.raw must_== age
    }
    "have an empty slice" in new age {
      age.slice must_== Point.empty
    }
  }
  s"$name one-dimensional sliced to 2014" should {
    trait ageS extends age {
      val ageS = age.slice(Point(year("2014")))
    }
    "have slice Point(2014)" in new ageS {
      ageS.slice must_== Point(year("2014"))
    }
    "have no dimension" in new ageS {
      ageS.dimensions must be empty
    }
    "contain one value (values, sparse, dense)" in new ageS {
      ageS.values must have size 1
      ageS.sparse must have size 1
      ageS.dense must have size 1
    }
    "return 33 for get(2014)" in new ageS {
      val at = year("2014")
      ageS.get(at) must_== Some(33)
      ageS.isDefinedAt(at) must beTrue
      ageS(at) must_== 33
    }
    "be undefined at 2013, 2015" in new ageS {
      ageS.isDefinedAt(year("2013")) must beFalse
      ageS.isDefinedAt(year("2015")) must beFalse
    }
    "raw return the original cube" in new ageS {
      ageS.raw must_== age
    }
    "raw have 3 values" in new ageS {
      ageS.raw.values must have size 3
    }
  }
  s"$name one-dimensional diced to 2014" should {
    trait ageD extends age {
      val ageD = age.dice(year, _ == year("2014"))
    }
    "have one dimension (year)" in new ageD {
      ageD.dimensions must_== Set(year)
    }
    "have empty slice" in new ageD {
      ageD.slice must_== Point.empty
    }
    "contain one value (values, sparse, dense)" in new ageD {
      ageD.values must have size 1
      ageD.sparse must have size 1
      ageD.dense must have size 1
    }
    "return 33 for get(2014)" in new ageD {
      val at = year("2014")
      ageD.get(at) must_== Some(33)
      ageD.isDefinedAt(at) must beTrue
      ageD(at) must_== 33
    }
    "be undefined at 2013, 2015" in new ageD {
      ageD.isDefinedAt(year("2013")) must beFalse
      ageD.isDefinedAt(year("2015")) must beFalse
    }
    "as raw return the original cube" in new ageD {
      ageD.raw must_== age
    }
    ".raw have 3 values" in new ageD {
      ageD.raw.values must have size 3
    }
  }

  trait color extends Scope {
    val color = makeDimension("color", List("red", "green", "white"))
    def color(v: String): Coordinate = coordOf(color, v)
  }
  trait location extends Scope {
    val location = makeDimension("location", List("Bern", "NY", "Shanghai"))
    def location(v: String): Coordinate = coordOf(location, v)
  }
  trait product extends Scope {
    val product = makeDimension("product", List("shirt", "socks"))
    def product(v: String): Coordinate = coordOf(product, v)
  }
  trait sales extends year with color with location with product {
    private def salesData = {
      val d = ("shirt", "Bern", 20, 15, 100) :: ("shirt", "NY", 10, 5, 2) :: ("shirt", "Shanghai", 0, 1, 0) ::
        ("socks", "Bern", 100, 120, 555) :: ("socks", "NY", 200, 10, 1500) :: Nil
      d.flatMap { v ⇒
        (Point(product(v._1), location(v._2), color("red")), v._3) ::
          (Point(product(v._1), location(v._2), color("green")), v._4) ::
          (Point(product(v._1), location(v._2), color("white")), v._5) :: Nil
      }
    }.toMap

    /** Three dimensional cube containing the number of sold units for color, location and product. */
    val sales = makeSales(color, location, product, salesData)
  }

  s"$name sales (three-dimensional)" should {
    "contain 15 values" in new sales {
      sales.values must have size 15
      sales.sparse must have size 15
    }
    "contain 18 points" in new sales {
      sales.dense must have size 18
    }
    "contain 15 defined points" in new sales {
      sales.dense.filter(_._2.isDefined) must have size 15
    }
    "contain 3 empty points" in new sales {
      sales.dense.filter(_._2.isEmpty) must have size 3
    }
    "have a total of 2638 products sold (when aggregating values)" in new sales {
      sales.values.reduce(_ + _) must_== 2638
    }
    "have a total of 2638 products sold (when aggregating sparse)" in new sales {
      sales.sparse.map(_._2).reduce(_ + _) must_== 2638
    }
    "have a total of 2638 products sold (when aggregating dense)" in new sales {
      sales.dense.map(_._2).flatten.reduce(_ + _) must_== 2638
    }
    "have values 20, 15, 100 for shirts in Bern" in new sales {
      val slice = sales.slice(product("shirt") + location("Bern"))
      slice.values.toSet must_== Set(20, 15, 100)
    }
    "have sparses 20, 15, 100 for shirts in Bern" in new sales {
      val p = product("shirt") + location("Bern")
      val slice = sales.slice(p)
      slice.sparse.toMap must_== Map(
        (p + color("red"), 20),
        (p + color("green"), 15),
        (p + color("white"), 100))
    }
    "have denses Some(20), Some(15), Some(100) for shirts in Bern" in new sales {
      val p = product("shirt") + location("Bern")
      val slice = sales.slice(p)
      slice.dense.toMap must_== Map(
        (p + color("red"), Some(20)),
        (p + color("green"), Some(15)),
        (p + color("white"), Some(100)))
    }
    "have sold 555 white socks in Bern" in new sales {
      val at = Point(product("socks"), location("Bern"), color("white"))
      sales.get(at) must beSome(555)
      sales.isDefinedAt(at) must beTrue
      sales(at) must_== 555
    }
    "have sold 5 green shirts in NY" in new sales {
      val at = Point(product("shirt"), location("NY"), color("green"))
      sales.get(at) must beSome(5)
      sales.isDefinedAt(at) must beTrue
      sales(at) must_== 5
    }
    "have no data for white sock sales in shanghai" in new sales {
      sales.get(Point(product("socks"), location("Shanghai"), color("white"))) must beNone
    }
    "have no aggregated data in products" in new sales {
      sales.get(product.all.head) must beNone
      sales.isDefinedAt(product.all.head) must beFalse
    }
    "have no aggregated data for Bern/shirt" in new sales {
      sales.get(Point(product("shirt"), location("Bern"))) must beNone
    }
    "have product, location and color as dimension" in new sales {
      sales.dimensions must_== Set(product, location, color)
    }
    "as slice return empty" in new sales {
      sales.slice must_== Point.empty
    }
    "as raw return itself" in new sales {
      sales.raw must_== sales
    }
  }
  s"$name sales (three-dimensional) sliced to Bern" should {
    trait sales2 extends sales {
      val sales2 = sales.slice(location("Bern"))
    }
    "as raw return original cube" in new sales2 {
      sales2.raw must_== sales
    }
    "as slice return Point(Bern)" in new sales2 {
      sales2.slice must_== Point(location("Bern"))
    }
    "have the dimensions product and color" in new sales2 {
      sales2.dimensions must_== Set(product, color)
    }
    "contain 6 values" in new sales2 {
      sales2.values must have size 6
      sales2.sparse must have size 6
    }
    "contain 6 defined values" in new sales2 {
      sales2.dense must have size 6
      sales2.dense.filter(_._2.isDefined) must have size 6
    }
    "have a total of 910 products sold" in new sales2 {
      sales2.values.reduce(_ + _) must_== 910
      sales2.sparse.map(_._2).reduce(_ + _) must_== 910
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 910
    }
    "be undefined for green shirts in NY" in new sales2 {
      val at = Point(product("shirt"), location("NY"), color("green"))
      sales2.get(at) must beNone
      sales2.isDefinedAt(at) must beFalse
    }
    "have no aggregated data for Bern/shirt" in new sales2 {
      sales2.get(Point(product("shirt"), location("Bern"))) must beNone
    }
    "have sold 555 white socks in Bern" in new sales2 {
      val at = Point(product("socks"), location("Bern"), color("white"))
      sales2.get(at) must beSome(555)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 555
    }
  }
  s"$name sales (three-dimensional) sliced to Bern/socks" should {
    trait sales2 extends sales {
      val sp = Point(location("Bern"), product("socks"))
      val sales2 = sales.slice(sp)
    }
    "as raw return original cube" in new sales2 {
      sales2.raw must_== sales
    }
    "as slice return Point(Bern, socks)" in new sales2 {
      sales2.slice must_== sp
    }
    "have the dimension color" in new sales2 {
      sales2.dimensions must_== Set(color)
    }
    "contain 3 values" in new sales2 {
      sales2.values must have size 3
      sales2.sparse must have size 3
    }
    "contain 3 defined values" in new sales2 {
      sales2.dense must have size 3
      sales2.dense.filter(_._2.isDefined) must have size 3
    }
    "have a total of 775 products sold" in new sales2 {
      sales2.values.reduce(_ + _) must_== 775
      sales2.sparse.map(_._2).reduce(_ + _) must_== 775
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 775
    }
    "be undefined for green shirts in NY" in new sales2 {
      val at = Point(product("shirt"), location("NY"), color("green"))
      sales2.get(at) must beNone
      sales2.isDefinedAt(at) must beFalse
    }
    "have no aggregated data for Bern/socks" in new sales2 {
      sales2.get(Point(product("socks"), location("Bern"))) must beNone
    }
    "have sold 555 white socks in Bern" in new sales2 {
      val at = Point(product("socks"), location("Bern"), color("white"))
      sales2.get(at) must beSome(555)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 555
    }
  }
  s"$name sales (three-dimensional) sliced to green socks" should {
    trait sales2 extends sales {
      val sp = Point(color("green"), product("socks"))
      val sales2 = sales.slice(sp)
    }
    "as raw return original cube" in new sales2 {
      sales2.raw must_== sales
    }
    "as slice return Point(green, socks)" in new sales2 {
      sales2.slice must_== sp
    }
    "have the dimension location" in new sales2 {
      sales2.dimensions must_== Set(location)
    }
    "contain 2 values" in new sales2 {
      sales2.values must have size 2
      sales2.sparse must have size 2
    }
    "contain 2 defined values" in new sales2 {
      sales2.dense.filter(_._2.isDefined) must have size 2
    }
    "contain 3 points" in new sales2 {
      sales2.dense must have size 3
    }
    "have a total of 130 products sold" in new sales2 {
      sales2.values.reduce(_ + _) must_== 130
      sales2.sparse.map(_._2).reduce(_ + _) must_== 130
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 130
    }
    "have sold 120 green socks in Bern" in new sales2 {
      val at = Point(product("socks"), location("Bern"), color("green"))
      sales2.get(at) must beSome(120)
      sales2.isDefinedAt(at) must beTrue
      sales2(at) must_== 120
    }
  }
  s"$name sales (three-dimensional) diced to green and red" should {
    trait sales2 extends sales {
      val sales2 = sales.dice(color, c ⇒ color.render(c) == "green" || color.render(c) == "red")
    }
    "as raw return original cube" in new sales2 {
      sales2.raw must_== sales
    }
    "as slice return Point.empty" in new sales2 {
      sales2.slice must_== Point.empty
    }
    "have product, location and color as dimension" in new sales2 {
      sales2.dimensions must_== Set(product, location, color)
    }
    "contain 10 values" in new sales2 {
      sales2.values must have size 10
      sales2.sparse must have size 10
      sales2.dense.filter(_._2.isDefined) must have size 10
    }
    "contain 12 points" in new sales2 {
      sales2.dense must have size 12
    }
    "have a total of 481 products sold" in new sales2 {
      sales2.values.reduce(_ + _) must_== 481
      sales2.sparse.map(_._2).reduce(_ + _) must_== 481
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 481
    }
  }
  s"$name sales (three-dimensional) sliced to socks and diced to green and red" should {
    trait sales2 extends sales {
      val sales2 = sales.
        dice(color, c ⇒ color.render(c) == "green" || color.render(c) == "red").
        slice(product("socks"))
    }
    "as raw return original cube" in new sales2 {
      sales2.raw must_== sales
    }
    "as slice return socks" in new sales2 {
      sales2.slice must_== Point(product("socks"))
    }
    "have location and color as dimension" in new sales2 {
      sales2.dimensions must_== Set(location, color)
    }
    "contain 4 values" in new sales2 {
      sales2.values must have size 4
      sales2.sparse must have size 4
      sales2.dense.filter(_._2.isDefined) must have size 4
    }
    "contain 6 points" in new sales2 {
      sales2.dense must have size 6
    }
    "have a total of 430 products sold" in new sales2 {
      sales2.values.reduce(_ + _) must_== 430
      sales2.sparse.map(_._2).reduce(_ + _) must_== 430
      sales2.dense.map(_._2).flatten.reduce(_ + _) must_== 430
    }
  }

  trait salesString extends sales {
    val salesString = sales.map(_.toString)
  }
  s"$name sales mapped to String" should {
    "contain 15 values" in new salesString {
      salesString.values must have size 15
      salesString.sparse must have size 15
    }
    "contain 18 points" in new salesString {
      salesString.dense must have size 18
    }
    "contain 15 defined points" in new salesString {
      salesString.dense.filter(_._2.isDefined) must have size 15
    }
    "contain 3 empty points" in new salesString {
      salesString.dense.filter(_._2.isEmpty) must have size 3
    }
    "have value '555' for white socks in Bern" in new salesString {
      val at = Point(product("socks"), location("Bern"), color("white"))
      salesString.get(at) must beSome("555")
      salesString.isDefinedAt(at) must beTrue
      salesString(at) must_== "555"
    }
    "have value '5' for green shirts in NY" in new salesString {
      val at = Point(product("shirt"), location("NY"), color("green"))
      salesString.get(at) must beSome("5")
      salesString.isDefinedAt(at) must beTrue
      salesString(at) must_== "5"
    }
    "have values '20', '15', '100' for shirts in Bern" in new salesString {
      val slice = salesString.slice(product("shirt") + location("Bern"))
      slice.values.toSet must_== Set("20", "15", "100")
    }
    "have sparses '20', '15', '100' for shirts in Bern" in new salesString {
      val p = product("shirt") + location("Bern")
      val slice = salesString.slice(p)
      slice.sparse.toMap must_== Map(
        (p + color("red"), "20"),
        (p + color("green"), "15"),
        (p + color("white"), "100"))
    }
    "have denses Some('20'), Some('15'), Some('100') for shirts in Bern" in new salesString {
      val p = product("shirt") + location("Bern")
      val slice = salesString.slice(p)
      slice.dense.toMap must_== Map(
        (p + color("red"), Some("20")),
        (p + color("green"), Some("15")),
        (p + color("white"), Some("100")))
    }
    "have no data for white sock sales in shanghai" in new salesString {
      salesString.get(Point(product("socks"), location("Shanghai"), color("white"))) must beNone
    }
    "have no aggregated data in products" in new salesString {
      salesString.get(product.all.head) must beNone
      salesString.isDefinedAt(product.all.head) must beFalse
    }
    "have no aggregated data for Bern/shirt" in new salesString {
      salesString.get(Point(product("shirt"), location("Bern"))) must beNone
    }
    "have product, location and color as dimension" in new salesString {
      salesString.dimensions must_== Set(product, location, color)
    }
    "as slice return empty" in new salesString {
      salesString.slice must_== Point.empty
    }
    "as raw return itself" in new salesString {
      salesString.raw must_== salesString
    }
  }
}

class MapCubeSpec extends Specification {
  include(new CubeTck("MapCube") {
    override def makeDimension(name: String, data: List[String]) = ListDimension(name, data: _*)
    override def makeAge(year: Dimension, data: Map[Point, Int]) = MapCube(data)
    override def makeSales(color: Dimension, location: Dimension, product: Dimension, data: Map[Point, Int]) = MapCube(data)
  })
}