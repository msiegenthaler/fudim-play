package domain

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import cube.TestFixtures._
import cube.{Cube, Coordinate, Aggregator, Dimension}

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
    val additionCube = {
      val formula = additionFormula("product" :: "sum" :: Nil, german :: english :: Nil)
      FormulaCube(formula, cubes)
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
  }
}
