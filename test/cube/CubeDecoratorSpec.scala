package cube

import org.specs2.mutable.Specification

class CubeDecoratorSpec extends Specification {

  val german = ListDimension("german", "eins", "zwei", "drei")
  val english = ListDimension("english", "one", "two", "three")

  val productCube = {
    val data = for {
      (cg, vg) ← german.all.zipWithIndex
      (ce, ve) ← english.all.zipWithIndex
    } yield (Point(cg, ce), (vg + 1) * (ve + 1))
    MapCube(data.toMap)
  }

  private val zweiTwo = Point(german.coordOf("zwei"), english.coordOf("two"))

  "decoration of cube with Noop" should {
    val dec = CubeDecorator(productCube, CubeDecorator.Noop[Int])
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
    "have no value at zwei" in {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 36" in {
      dec.values.reduce(_ + _) must_== (36)
    }
  }
}