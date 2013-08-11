package cube

import org.specs2.mutable.Specification

abstract class DimensionTck(name: String, oneTwoThree: Dimension) extends Specification {
  s"$name oneTwoThree" should {
    "have a name" in {
      oneTwoThree.name must_== "oneTwoThree"
    }
    "return its name in toString" in {
      oneTwoThree.toString must_== oneTwoThree.name
    }
    "contain three coordinates" in {
      oneTwoThree.all must have size 3
    }
    "contain coordinates that reference that same dimension" in {
      oneTwoThree.all.map(_.dimension).toSet must_== Set(oneTwoThree)
    }
    "render value 'one' for the first coordinate" in {
      oneTwoThree.render(oneTwoThree.all.head) must_== "one"
    }
    "render a value for every coordinate" in {
      oneTwoThree.all.map(oneTwoThree.render) must containTheSameElementsAs(List("one", "two", "three"))
    }
    "return the same for d.all.map(c => (c, d.render(c))) and for d.values" in {
      oneTwoThree.all.map(c ⇒ (c, oneTwoThree.render(c))) must_== oneTwoThree.values
    }
  }
}

class ListDimensionSpec extends Specification {
  val d = ListDimension("oneTwoThree", "one" :: "two" :: "three" :: Nil)
  include(new DimensionTck("ListDimension", d) {})

  "ListDimension [one,two,three]" should {
    "still have the same name after +('four')" in {
      (d + "four").name must_== "oneTwoThree"
    }
    "return a dimension with 4 values after +('four')" in {
      (d + "four").all must have size 4
    }
    "have the last value as 'four' after +('four')" in {
      (d + "four").values.last._2 must_== "four"
    }

    "still have the same name after -('one')" in {
      (d - "one").name must_== "oneTwoThree"
    }
    "return a dimension with 2 values after -('one')" in {
      (d - "one").all must have size 2
    }
    "have the first value as 'two' after -('one')" in {
      (d - "one").values.head._2 must_== "two"
    }
  }
}