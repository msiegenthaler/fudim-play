package support

import java.nio.ByteBuffer
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class BloomFilterSpec extends Specification {
  "BloomFilterConfig with capacity 1438 and 10 hashes" should {
    "have p of 0.001 on 100 items" in {
      BloomFilterConfig(1438, 10).falsePositives(100) must beCloseTo(0.001d, 1E-4)
    }
  }

  "BloomFilterConfig with capacity 9586 and 7 hashes" should {
    "have p of 0.01 with 1000 items" in {
      BloomFilterConfig(9586, 7).falsePositives(1000) must beCloseTo(0.01d, 1E-4)
    }
  }

  "BloomFilterConfig for 1000 items with p=0.01" should {
    "have capacity 9586 and 7 hashes" in {
      val c = BloomFilterConfig.forFalsePositives(1000, 0.01d)
      c.capacity must_== 9586
      c.hashCount must_== 7
    }
  }

  "BloomFilterConfig" should {
    "be consistent" in {
      (100 to 10000 by 10).map { n =>
        BloomFilterConfig.forFalsePositives(n, 0.1d).falsePositives(n) must beCloseTo(0.1d, 1E-2)
        BloomFilterConfig.forFalsePositives(n, 0.01d).falsePositives(n) must beCloseTo(0.01d, 1E-3)
        BloomFilterConfig.forFalsePositives(n, 0.001d).falsePositives(n) must beCloseTo(0.001d, 1E-4)
      }
    }
  }

  trait xvalues extends Scope {
    def value(d: Int) = ByteBuffer.allocate(4).putInt(d).array
  }
  trait hundred extends xvalues {
    val filter = BloomFilter(100, 0.001d) ++ (1 to 100).map(value)
  }
  trait ten extends xvalues {
    val filter = BloomFilter(100, 0.001d) ++ (1 to 10).map(value)
  }
  trait union extends ten {
    val filter2 = BloomFilter(100, 0.001d) ++ (8 to 15).map(value)
    val union = filter union filter2
  }
  trait intersect extends ten {
    val filter2 = BloomFilter(100, 0.001d) ++ (8 to 15).map(value)
    val intersect = filter intersect filter2
  }

  "BloomFilter that contains the items 1..100" should {
    "return true maybeContains 1..100" in new hundred {
      (1 to 100).map(value).map { v =>
        filter.maybeContains(v) must beTrue
      }
    }
    "have not have more than 120 false positive in 101 to 10000" in new hundred {
      val count = (101 to 100000).map(value).filter(filter.maybeContains).size
      count must beLessThanOrEqualTo(120)
    }
    "have approx item count of 100+-2" in new hundred {
      filter.approxNumberOfItems must beCloseTo(100d, 2)
    }

    "return true maybeContains 1..100 when using BloomFilterCheck" in new hundred {
      (1 to 100).map(value).map(BloomFilterCheck(_, filter.config)).map { v =>
        filter.maybeContains(v) must beTrue
      }
    }
    "have not have more than 120 false positive in 101 to 10000 using BloomFilterCheck" in new hundred {
      val count = (101 to 100000).map(value).map(BloomFilterCheck(_, filter.config)).filter(filter.maybeContains).size
      count must beLessThanOrEqualTo(120)
    }
  }

  "BloomFilter that contains [1..10] unioned with [8..15]" should {
    "contain approx 15 items" in new union {
      union.approxNumberOfItems must beCloseTo(15d, 1)
    }
    "return true for maybeContains 1..15" in new union {
      (1 to 15).map(value).map { v =>
        union.maybeContains(v) must beTrue
      }
    }
    "have not have more than 5 false positive in 16 to 10000" in new union {
      val count = (16 to 100000).map(value).filter(union.maybeContains).size
      count must beLessThanOrEqualTo(5)
    }
  }

  "BloomFilter that contains [1..10] intersected with [8..15]" should {
    "contain approx 3 items" in new intersect {
      intersect.approxNumberOfItems must beCloseTo(3d, 1)
    }
    "return true for maybeContains 8..10" in new intersect {
      (8 to 10).map(value).map { v =>
        intersect.maybeContains(v) must beTrue
      }
    }
    "have not have more than 5 false positive in 0..7 and 11 to 10000" in new intersect {
      val count = ((0 to 7) ++ (11 to 100000)).map(value).filter(intersect.maybeContains).size
      count must beLessThanOrEqualTo(5)
    }
  }
}
