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

  /** String value of the coordinate. */
  def render(c: Coordinate): String
  /** all coordinates along with their rendered value. */
  def values: Seq[(Coordinate, String)]

  /** Add a value to the dimension (at last index). */
  def add(value: String): Coordinate
  /** Adds a value to the dimension directly after another value (use None to insert as first). */
  def add(value: String, after: Option[Coordinate]): Coordinate

  override def toString = name
}