package domain

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import cube.TestFixtures._
import cube._

class FormulaCubeSpec extends Specification {
  trait additionCube extends sumCube with productCube {
    val intType = new DataType[Int] {
      override val name = "Int"
      override val tpe = classOf[Int]
    }
    val cubes = new Cubes {
      val cs: Map[CubeRef[_], Cube[_]] = Map(
        CubeRef("product", intType) -> productCube,
        CubeRef("sum", intType) -> sumCube)
      def refs = cs.keySet
      def get[T](ref: CubeRef[T]) = cs.get(ref).map(_.asInstanceOf[Cube[T]])
    }
    def additionFormula(of: Traversable[String], over: Traversable[Dimension]) = {
      def sum(vs: Traversable[Option[Int]]): Option[Int] =
        Some(vs.filter(_.isDefined).map(_.get).foldLeft(0)(_ + _))
      Formulas.pointFold(sum, intType)(of, intType, over)
    }
    val additionFormula: Formula[Int] = additionFormula("product" :: "sum" :: Nil, german :: english :: Nil)
    val additionCube = {
      FormulaCube(additionFormula, cubes)
    }
  }

  "addition formula of two cubes" should {
    "be 3 (1+2) at one/eins" in new additionCube {
      additionCube.get(einsOne) must beSome(3)
    }
    "be 8 (4+4) at two/zwei" in new additionCube {
      additionCube.get(zweiTwo) must beSome(8)
    }
    "be None at two" in new additionCube {
      additionCube.get(english.coordOf("two")) must beNone
    }
    "be None at eins" in new additionCube {
      additionCube.get(german.coordOf("eins")) must beNone
    }

    "unaply to the formula" in new additionCube {
      FormulaCube.unapply(additionCube) must_== Some(additionFormula)
    }
  }

  "FormulaCube" should {
    "unapply non-formula cubes to none" in new productCube {
      FormulaCube.unapply(productCube) must beNone
    }
  }

}
