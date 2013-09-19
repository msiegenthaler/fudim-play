package cube

sealed trait Coordinate {
  /** Dimension of the coordinate. */
  val dimension: Dimension
  /** Unique identifier with the dimension. */
  val id: Long
}
trait CoordinateFactory {
  protected[this] def coordinate(dimension: Dimension, id: Long): cube.Coordinate = new Coordinate(dimension, id)

  private class Coordinate(override val dimension: Dimension, override val id: Long) extends cube.Coordinate {
    override def equals(o: Any) = o match {
      case other: Coordinate ⇒ other.dimension == dimension && other.id == id
      case _ ⇒ false
    }
    override def hashCode = dimension.hashCode ^ id.hashCode
    override def toString = s"$dimension=$id"
  }
}

trait Dimension {
  /** Unique name of the dimension. */
  def name: String

  /** Values of this dimensions (ordered). */
  def all: Seq[Coordinate]
  /** Value at the specified coordinate. */
  def get(id: Long): Option[Coordinate] = all.find(_.id == id)

  /**
   * String value of the coordinate.
   * Throws an IllegalArgumentException if the coordinate is not of this dimension (or does not exist).
   */
  def render(c: Coordinate): String
  /** all coordinates along with their rendered value. */
  def values: Seq[(Coordinate, String)]

  override def toString = name
}

trait DimensionRepository {
  def all: TraversableOnce[Dimension]
  def get(name: String): Option[Dimension] = all.find(_.name == name)
}
