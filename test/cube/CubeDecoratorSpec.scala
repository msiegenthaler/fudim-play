package cube

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import support.JsonMapperRepository
import support.ObjectJsonMapper
import TestFixtures._

class CubeDecoratorSpec extends Specification {
  trait decNoop extends productCube {
    val dec = CubeDecorator(productCube, CubeDecorators.noop[Int])
  }
  "decoration of cube with Noop" should {
    "still have 9 values" in new decNoop {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "still have 9 points" in new decNoop {
      dec.dense must have size 9
    }
    "have 4 at zwei/two" in new decNoop {
      dec.get(zweiTwo) must beSome(4)
    }
    "have no value at zwei" in new decNoop {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 36" in new decNoop {
      dec.values.reduce(_ + _) must_== (36)
    }

    "be unapplyable to the apply-parameters" in new decNoop {
      CubeDecorator.unapply(dec) must_== Some(productCube, CubeDecorators.noop)
    }
    "have the decorator to be removable via undecorate" in new decNoop {
      CubeDecorator.undecorate(dec) must_== productCube
    }
    "have the decorator to be removable completely via undecorate" in new decNoop {
      CubeDecorator.undecorateComplete(dec) must_== productCube
    }
    "have the decorator to be removable completely via undecorate even when decorated again" in new decNoop {
      val dec2 = CubeDecorator(dec, CubeDecorators.noop[Int])
      CubeDecorator.undecorateComplete(dec2) must_== productCube
    }
  }

  trait decPlusOne extends productCube {
    val dec = CubeDecorator(productCube, CubeDecorators.mapValue[Int](_ + 1))
  }
  "decoration of cube with '+1'" should {
    "still have 9 values" in new decPlusOne {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "still have 9 points" in new decPlusOne {
      dec.dense must have size 9
    }
    "have 5 at zwei/two" in new decPlusOne {
      dec.get(zweiTwo) must beSome(5)
    }
    "have no value at zwei" in new decPlusOne {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 45" in new decPlusOne {
      dec.values.reduce(_ + _) must_== (45)
    }
  }

  trait decOneNone extends productCube {
    val dec = CubeDecorator(productCube, CubeDecorators.mapValueOption[Int](_ match {
      case Some(1) ⇒ None
      case other ⇒ other
    }))
  }
  "decoration of cube with 1 => None" should {
    "have 8 values" in new decOneNone {
      dec.values must have size 8
      dec.sparse must have size 8
      dec.dense.filter(_._2.isDefined) must have size 8
    }
    "have one undefined point" in new decOneNone {
      dec.dense.filter(_._2.isEmpty) must have size 1
    }
    "still have 9 points" in new decOneNone {
      dec.dense must have size 9
    }
    "have 4 at zwei/two" in new decOneNone {
      dec.get(zweiTwo) must beSome(4)
    }
    "have no value at zwei" in new decOneNone {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 35" in new decOneNone {
      dec.values.reduce(_ + _) must_== (35)
    }
    "have no value at eins/one" in new decOneNone {
      dec.get(einsOne) must beNone
    }
  }

  trait decNone10 extends hasDiffCube {
    val dec = CubeDecorator(hasDiffCube, CubeDecorators.mapValueOption[Int](_ match {
      case None ⇒ Some(10)
      case other ⇒ other
    }))
  }
  "decoration of cube with None => 10" should {
    "have 9 values" in new decNone10 {
      dec.values must have size 9
      dec.sparse must have size 9
      dec.dense.filter(_._2.isDefined) must have size 9
    }
    "have 6 values before" in new decNone10 {
      hasDiffCube.values must have size 6
    }
    "still have 9 points" in new decNone10 {
      dec.dense must have size 9
    }
    "have 10 at zwei/two" in new decNone10 {
      dec.get(zweiTwo) must beSome(10)
    }
    "have no value at zwei" in new decNone10 {
      dec.get(german.coordOf("zwei")) must beNone
    }
    "have a value sum of 30" in new decNone10 {
      dec.values.reduce(_ + _) must_== (30)
    }
    "have value 10 at eins/one" in new decNone10 {
      dec.get(einsOne) must beSome(10)
    }
    "have value -1 at eins/two" in new decNone10 {
      dec.get(einsTwo) must beSome(-1)
    }
  }

  trait serializableDecorator extends Scope {
    val decorator = CubeDecorators.mapValue[Int](_ + 1)
    val decoratorMapper: JsonCubeDecoratorMapper = ObjectJsonMapper("addOne", decorator)
    val decoratorRepo = new JsonCubeDecoratorMapperRepository {
      override val mappers = decoratorMapper :: Nil
    }
  }
  trait serializableCube extends serializableDecorator with productCube {
    val cubeMapper: JsonCubeMapper = ObjectJsonMapper("productCube", productCube)
    val cubeRepo = new JsonCubeMapperRepository {
      override val mappers = cubeMapper :: Nil
    }
    val mapper = CubeDecorator.json(decoratorRepo, cubeRepo)
    val cube = CubeDecorator(productCube, decorator)
  }
  "CubeDecorator" should {
    "if decorator and cube are serializable (precond)" in new serializableCube {
      decoratorRepo.serialize(decorator).isSuccess must beTrue
      cubeRepo.serialize(productCube).isSuccess must beTrue
    }
    "be serializable to json" in new serializableCube {
      mapper.serializer(cube).isSuccess must beTrue
    }
    "be reparsable" in new serializableCube {
      val p = for {
        json ← mapper.serializer(cube)
        c ← mapper.parser(json)
      } yield c
      p.isSuccess must beTrue

      p.toOption.get match {
        case CubeDecorator(u, d) ⇒
          u must_== productCube
          d must_== decorator
      }
    }
  }
}