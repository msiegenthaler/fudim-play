package cube

import org.specs2.mutable.Specification

class AggregatorSpec extends Specification {
  val german = ListDimension("german", "eins", "zwei", "drei")
  val english = ListDimension("english", "one", "two", "three")

  val productCube = {
    val data = for {
      (cg, vg) ← german.all.zipWithIndex
      (ce, ve) ← english.all.zipWithIndex
    } yield (Point(cg, ce), ((vg + 1) * (ve + 1)).toLong)
    MapCube(data.toMap)
  }

  private val einsOne = Point(german.coordOf("eins"), english.coordOf("one"))
  private val einsTwo = Point(german.coordOf("eins"), english.coordOf("two"))
  private val zweiTwo = Point(german.coordOf("zwei"), english.coordOf("two"))

  "aggregation of cube with sum" should {
    val dec = CubeDecorator(productCube, Aggregators.sum)

    "have value 18 at drei" in {
      dec.get(german.coordOf("drei")) must beSome(18)
    }
    "have value 12 at zwei" in {
      dec.get(german.coordOf("zwei")) must beSome(12)
    }
    "have value 6 at eins" in {
      dec.get(german.coordOf("eins")) must beSome(6)
    }
    "have value 6 at one" in {
      dec.get(english.coordOf("one")) must beSome(6)
    }
    "have value 36 at Point.empty" in {
      dec.get(Point.empty) must beSome(36)
    }
    "still have 9 values" in {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "still have 9 points" in {
      dec.dense must have size 9
    }
    "have 4 at zwei/two" in {
      dec.get(zweiTwo) must beSome(4)
    }
    "have a value sum of 36" in {
      dec.values.reduce(_ + _) must_== (36)
    }
  }

  "Aggregator" should {
    "be unapplyable from Decorator" in {
      val aggr = Aggregators.sum
      val dec: CubeDecorator[Long] = aggr
      Aggregator.unapply(dec) must_== Some(aggr)
    }

  }
}