package cube

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import TestFixtures._
import support._

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

  trait avgCube extends productCube {
    val cube = CubeDecorator(productCube, Aggregators.avgInt)
  }
  "aggregation of cube with avg" should {
    "have value 2 at eins" in new avgCube {
      cube.get(german.coordOf("eins")) must beSome(2)
    }
    "have value 2 at zwei" in new avgCube {
      cube.get(german.coordOf("zwei")) must beSome(4)
    }
    "have value 2 at one" in new avgCube {
      cube.get(english.coordOf("one")) must beSome(2)
    }
  }

  trait serializableAggregator extends Scope {
    val aggregator = Aggregators.sumInt
    val aggregatorMapper: JsonMapper[Aggregator[_]] = ObjectJsonMapper("sumInt", aggregator)
    val aggregatorRepo = new JsonMapperRepository[Aggregator[_]] {
      override val mappers = aggregatorMapper :: Nil
    }
  }
  trait serializableDecorator extends serializableAggregator {
    val decoratorRepo = new JsonCubeDecoratorMapperRepository {
      override val mappers = Aggregator.json(aggregatorRepo) :: Nil
    }
  }
  "Aggregator" should {
    "be unapplyable from Decorator" in {
      val aggr = Aggregators.sumInt
      val dec: CubeDecorator[Int] = aggr
      Aggregator.unapply(dec) must_== Some(aggr)
    }

    "be serializable to json (if aggregator is serializable)" in new serializableDecorator {
      val dec: CubeDecorator[Int] = aggregator
      decoratorRepo.serialize(dec).isSuccess must beTrue
    }
    "be reparsable from json (if aggregator is serializable)" in new serializableDecorator {
      val dec: CubeDecorator[Int] = aggregator
      val p = for {
        json ← decoratorRepo.serialize(dec)
        c ← decoratorRepo.parse(json)
      } yield c
      p.isSuccess must beTrue

      p.toOption.get match {
        case Aggregator(a) ⇒
          a must_== aggregator
      }
    }
  }
}
