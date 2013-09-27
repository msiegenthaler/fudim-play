package domain

import org.specs2.mutable.Specification
import cube._
import cube.TestFixtures._
import support.{JsonMapperRepository, ObjectJsonMapper, JsonMapper}

class FormulaCubeSpec extends Specification {
  trait additionCube extends sumCube with productCube with TestFixtures.dataTypes {
    val cubes = new Cubes {
      val cs: Map[CubeRef[_], Cube[_]] = Map(
        CubeRef("product", intType) -> productCube,
        CubeRef("sum", intType) -> sumCube)
      def refs = cs.keySet
      def get[T](ref: CubeRef[T]) = cs.get(ref).map(_.asInstanceOf[Cube[T]])
    }
    def additionFormula(of: Seq[String], over: Traversable[Dimension]) = {
      def sum(vs: Traversable[Option[Int]]): Option[Int] =
        Some(vs.filter(_.isDefined).map(_.get).foldLeft(0)(_ + _))
      PointFoldFormula(sum, intType)(of, intType, over)
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

  trait serializableFormula extends additionCube {
    val formulaRepo = new JsonFormulaMapperRepository {
      private val add: JsonFormulaMapper = ObjectJsonMapper("addition", additionFormula)
      override val mappers = add :: Nil
    }
  }
  trait serializableCube extends serializableFormula {
    val cubeRepo = new JsonCubeMapperRepository {
      private val fcm: JsonCubeMapper = FormulaCube.json(formulaRepo)(cubes)
      override val mappers = fcm :: Nil
    }
  }
  "FormulaCube" should {
    "unapply non-formula cubes to none" in new productCube {
      FormulaCube.unapply(productCube) must beNone
    }

    "be serializable to json (if formula is serializable)" in new serializableCube {
      cubeRepo.serialize(additionCube).isSuccess must beTrue
    }
    "be reparsable from json (if formula is serializable)" in new serializableCube {
      val p = for {
        json ← cubeRepo.serialize(additionCube)
        c ← cubeRepo.parse(json)
      } yield c
      p.isSuccess must beTrue
      p.toOption.get match {
        case FormulaCube(a) ⇒
          a must_== additionFormula
      }
    }
  }
}
