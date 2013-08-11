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
  val hasDiffCube = {
    val data = for {
      (cg, vg) ← german.all.zipWithIndex
      (ce, ve) ← english.all.zipWithIndex if ve != vg
    } yield (Point(cg, ce), (vg + 1) - (ve + 1))
    MapCube(data.toMap)
  }

  private val einsOne = Point(german.coordOf("eins"), english.coordOf("one"))
  private val einsTwo = Point(german.coordOf("eins"), english.coordOf("two"))
  private val zweiTwo = Point(german.coordOf("zwei"), english.coordOf("two"))

  "decoration of cube with Noop" should {
    val dec = CubeDecorator(productCube, CubeDecorators.noop[Int])
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

    "be unapplyable to the apply-parameters" in {
      CubeDecorator.unapply(dec) must_== Some(productCube, CubeDecorators.noop)
    }
    "have the decorator to be removable via undecorate" in {
      CubeDecorator.undecorate(dec) must_== productCube
    }
    "have the decorator to be removable completely via undecorate" in {
      CubeDecorator.undecorateComplete(dec) must_== productCube
    }
    "have the decorator to be removable completely via undecorate even when decorated again" in {
      val dec2 = CubeDecorator(dec, CubeDecorators.noop[Int])
      CubeDecorator.undecorateComplete(dec2) must_== productCube
    }
  }

  "decoration of cube with '+1'" should {
    val dec = CubeDecorator(productCube, CubeDecorators.mapValue[Int](_ + 1))
    "still have 9 values" in {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "still have 9 points" in {
      dec.dense must have size 9
    }
    "have 5 at zwei/two" in {
      dec.get(zweiTwo) must beSome(5)
    }
    "have no value at zwei" in {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 45" in {
      dec.values.reduce(_ + _) must_== (45)
    }
  }

  "decoration of cube with 1 => None" should {
    val dec = CubeDecorator(productCube, CubeDecorators.mapValueOption[Int](_ match {
      case Some(1) ⇒ None
      case other ⇒ other
    }))
    "have 8 values" in {
      dec.values must have size 8
      dec.sparse must have size 8
      dec.dense.filter(_._2.isDefined) must have size 8
    }
    "have one undefined point" in {
      dec.dense.filter(_._2.isEmpty) must have size 1
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
    "have a value sum of 35" in {
      dec.values.reduce(_ + _) must_== (35)
    }
    "have no value at eins/one" in {
      dec.get(einsOne) must beNone
    }
  }

  "decoration of cube with None => 10" should {
    val dec = CubeDecorator(hasDiffCube, CubeDecorators.mapValueOption[Int](_ match {
      case None ⇒ Some(10)
      case other ⇒ other
    }))
    "have 9 values" in {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "have 6 values before" in {
      hasDiffCube.values must have size 6
    }
    "still have 9 points" in {
      dec.dense must have size 9
    }
    "have 10 at zwei/two" in {
      dec.get(zweiTwo) must beSome(10)
    }
    "have no value at zwei" in {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 30" in {
      dec.values.reduce(_ + _) must_== (30)
    }
    "have value 10 at eins/one" in {
      dec.get(einsOne) must beSome(10)
    }
    "have value -1 at eins/two" in {
      dec.get(einsTwo) must beSome(-1)
    }
  }
}