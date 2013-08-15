package cube

import org.specs2.mutable.Specification
import TestFixtures._

class AggregatorSpec extends Specification {
  trait sumCube extends productCube {
    val cube = CubeDecorator(productCube, Aggregators.sumInt)
  }
  "aggregation of cube with sum" should {
    "have value 18 at drei" in new sumCube {
      cube.get(german.coordOf("drei")) must beSome(18)
    }
    "have value 12 at zwei" in new sumCube {
      cube.get(german.coordOf("zwei")) must beSome(12)
    }
    "have value 6 at eins" in new sumCube {
      cube.get(german.coordOf("eins")) must beSome(6)
    }
    "have value 6 at one" in new sumCube {
      cube.get(english.coordOf("one")) must beSome(6)
    }
    "have value 36 at Point.empty" in new sumCube {
      cube.get(Point.empty) must beSome(36)
    }
    "still have 9 values" in new sumCube {
      cube.values must have size 9
      cube.sparse must have size 9
      cube.dense.filter(_._2.isDefined) must have size 9
    }
    "still have 9 points" in new sumCube {
      cube.dense must have size 9
    }
    "have 4 at zwei/two" in new sumCube {
      cube.get(zweiTwo) must beSome(4)
    }
    "have a value sum of 36" in new sumCube {
      cube.values.reduce(_ + _) must_== (36)
    }
  }

  "Aggregator" should {
    "be unapplyable from Decorator" in {
      val aggr = Aggregators.sumInt
      val dec: CubeDecorator[Int] = aggr
      Aggregator.unapply(dec) must_== Some(aggr)
    }
  }
}