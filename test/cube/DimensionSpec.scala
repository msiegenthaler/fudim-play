package cube

import org.specs2.mutable.Specification

class DimensionSpec extends Specification {
  "ListDimension 'myDimension', [one,two,three]" should {
    val d = ListDimension("myDimension", "one" :: "two" :: "three" :: Nil)

    "have a name" in {
      d.name must_== "myDimension"
    }
    "return its name in toString" in {
      d.toString must_== d.name
    }
    "contain three coordinates" in {
      d.all must have size 3
    }
    "contain coordinates that reference that same dimension" in {
      d.all.map(_.dimension).toSet must_== Set(d)
    }
    "render value 'one' for the first coordinate" in {
      d.render(d.all.head) must_== "one"
    }
    "render a value for every coordinate" in {
      d.all.map(d.render) must containTheSameElementsAs(List("one", "two", "three"))
    }
    "return the same for d.all.map(c => (c, d.render(c))) and for d.values" in {
      d.all.map(c ⇒ (c, d.render(c))) must_== d.values
    }

    "still have the same name after +('four')" in {
      (d + "four").name must_== "myDimension"
    }
    "return a dimension with 4 values after +('four')" in {
      (d + "four").all must have size 4
    }
    "have the last value as 'four' after +('four')" in {
      (d + "four").values.last._2 must_== "four"
    }
  }
}