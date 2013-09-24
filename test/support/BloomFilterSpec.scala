package support

import org.specs2.mutable.Specification

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
}
