package support

case class BloomFilterConfig(/** Number of bits in the filter. */
                             capacity: Int,

                             /** Number of hash functions. */
                             hashCount: Int) {
  /** Probability of false positives [0,1]. */
  def falsePositives(numberOfItemsInFilter: Int) = {
    Math.pow(1 - Math.exp(-hashCount.toDouble * (numberOfItemsInFilter + 0.5) / (capacity - 1)), hashCount)
  }
}
object BloomFilterConfig {
  def forFalsePositives(expectedItemCount: Int, falsePositiveProbability: Double) = {
    val capacity = Math.ceil(expectedItemCount.toDouble * Math.log(falsePositiveProbability) / Math.log(1d / Math.pow(2d, Math.log(2d)))).toInt
    val hashCount = Math.round(Math.log(2d) * capacity / expectedItemCount).toInt
    BloomFilterConfig(capacity, hashCount)
  }
}
