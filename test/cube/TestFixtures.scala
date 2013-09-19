package cube

import org.specs2.specification.Scope

object TestFixtures {
  trait germanEnglish extends Scope {
    val german = ListDimension("german", "eins", "zwei", "drei")
    val english = ListDimension("english", "one", "two", "three")

    val einsOne = Point(german.coordOf("eins"), english.coordOf("one"))
    val einsTwo = Point(german.coordOf("eins"), english.coordOf("two"))
    val zweiTwo = Point(german.coordOf("zwei"), english.coordOf("two"))
  }

  trait productCube extends germanEnglish {
    val productCube = {
      val data = for {
        (cg, vg) ← german.all.zipWithIndex
        (ce, ve) ← english.all.zipWithIndex
      } yield (Point(cg, ce), (vg + 1) * (ve + 1))
      MapCube(data.toMap)
    }
  }

  trait sumCube extends germanEnglish {
    val sumCube = {
      val data = for {
        (cg, vg) ← german.all.zipWithIndex
        (ce, ve) ← english.all.zipWithIndex
      } yield (Point(cg, ce), vg + ve + 2)
      MapCube(data.toMap)
    }
  }

  trait hasDiffCube extends germanEnglish {
    val hasDiffCube = {
      val data = for {
        (cg, vg) ← german.all.zipWithIndex
        (ce, ve) ← english.all.zipWithIndex if ve != vg
      } yield (Point(cg, ce), (vg + 1) - (ve + 1))
      MapCube(data.toMap)
    }
  }
}
