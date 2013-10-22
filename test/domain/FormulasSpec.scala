package domain

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import support.{ JsonMapperRepository, ObjectJsonMapper, JsonMapper }
import PointFoldFormula.FoldFunction
import TestFixtures._
import cube.DimensionRepository

class FormulasSpec extends Specification {

  trait concatFunction {
    val concat: FoldFunction[String, String] = xs ⇒ {
      if (xs.isEmpty) None
      else Some(xs.foldLeft("")((a, e) ⇒ a + e.getOrElse("")))
    }
    val concatMapper: JsonMapper[FoldFunction[_, _]] = ObjectJsonMapper("concat", concat)
    val funRepo = new JsonMapperRepository[FoldFunction[_, _]] {
      override protected val mappers = concatMapper :: Nil
    }
  }
  trait repos extends concatFunction with dataTypes with cube.TestFixtures.germanEnglish {
    object dimRepo extends DimensionRepository {
      override val all = german :: english :: Nil
    }
    val pfMapper: JsonFormulaMapper = PointFoldFormula.json(dataTypeRepo, dimRepo, funRepo)
    object formulaRepo extends JsonFormulaMapperRepository {
      override protected val mappers = pfMapper :: Nil
    }
    val formula = PointFoldFormula(concat, stringType)("one" :: "two" :: Nil, stringType, german :: english :: Nil)
  }

  "PointFoldFormula" should {
    "be serializable to json (if function is)" in new repos {
      formulaRepo.serialize(formula).isSuccess must beTrue
    }
    "be reparsable from json (if function is serializable)" in new repos {
      val p = for {
        json ← formulaRepo.serialize(formula)
        _ = println(json)
        c ← formulaRepo.parse(json)
      } yield c
      println(p)
      p.isSuccess must beTrue
      val f = p.toOption.get
      f.dimensions must_== formula.dimensions
      f.references must_== f.references
    }
  }
}
