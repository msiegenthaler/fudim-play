package models

/** Point in a set of dimensions. Defines the coordinate in all this dimensions. */
class Point private (val coordinates: Set[Coordinate]) {
  /** Dimensions defined by this point. */
  val on = coordinates.map(_.dimension).toSet

  /** True if the point defines at least this dimensions. */
  def defines(ds: Traversable[Dimension]) = ds.filterNot(on.contains).isEmpty
  /** True if the point defines exactly this dimensions. */
  def definesExactly(ds: Traversable[Dimension]) = ds == on

  def coordinate(d: Dimension) = coordinates.find(_.dimension == d)

  def +(v: Coordinate): Point = {
    if (on.contains(v.dimension)) throw new IllegalArgumentException(s"Dimension ${v.dimension} already contained in $this")
    new Point(coordinates + v)
  }
  def -(v: Dimension): Point = new Point(coordinates.filterNot(_.dimension == v))
  def --(v: Traversable[Dimension]): Point = {
    val vs = v.toSet
    new Point(coordinates.filterNot(e ⇒ vs.contains(e.dimension)))
  }

  def mod(newValue: Coordinate): Point = {
    if (!on.contains(newValue.dimension)) throw new IllegalArgumentException(s"$this does not contain dimension ${newValue.dimension}")
    this - newValue.dimension + newValue
  }

  /** True if the other point has the same coordinate for every dimension defined by this point. */
  def contains(other: Point) = (coordinates -- other.coordinates).isEmpty

  override def equals(o: Any) = o match {
    case p: Point ⇒ coordinates == p.coordinates
    case _ ⇒ false
  }
  override def hashCode = coordinates.hashCode

  override def toString = "(" + coordinates.toList.sortBy(_.dimension.name).map(_.toString).mkString(", ") + ")"
}

object Point {
  /** A point in no dimension. Same as empty. */
  val singluarity = new Point(Set.empty)
  /** A point in no dimension. Same as singularity. */
  def empty = singluarity

  /** One dimensional point. */
  def apply(value: Coordinate*) = {
    val set = value.toSet
    if (value.size != set.size) throw new IllegalArgumentException(s"$value contains duplicate dimensions")
    new Point(set)
  }

  implicit def coordToPoint(c: Coordinate) = Point(c)
}