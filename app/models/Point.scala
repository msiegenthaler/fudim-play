package models

/** Point in a set of dimensions. */
class Point private (val values: Map[Dimension, String]) {
  /** Dimensions defined by this point. */
  def on = values.keys.toSet

  /** True if the point at least this dimensions. */
  def defines(ds: Traversable[Dimension]) = ds.filterNot(on.contains).isEmpty
  /** True if the point defines exactly this dimensions. */
  def definesExactly(ds: Traversable[Dimension]) = ds == on

  def valueOf(d: Dimension) = values.get(d)

  def +(v: (Dimension, String)): Point = {
    if (on.contains(v._1)) throw new IllegalArgumentException("Dimension " + v._1 + " already contained in " + this)
    new Point(values + v)
  }
  def -(v: Dimension): Point = {
    new Point(values - v)
  }

  def mod(d: Dimension, newValue: String): Point = {
    if (!on.contains(d)) throw new IllegalArgumentException(this.toString + " does not contain dimension " + d)
    new Point(values + (d -> newValue))
  }

  override def equals(o: Any) = o match {
    case p: Point ⇒ values == p.values
    case _ ⇒ false
  }
  override def hashCode = values.hashCode

  override def toString = "(" + values.toList.sortBy(_._1.name).map(v ⇒ v._1 + "=" + v._2).mkString(", ") + ")"
}

object Point {
  /** A point in no dimension. Same as empty. */
  val singluarity = new Point(Map.empty)
  /** A point in no dimension. Same as singularity. */
  def empty = singluarity

  def apply(d: Dimension, value: String) = new Point(Map(d -> value))
}