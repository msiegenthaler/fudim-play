package models

class Point private (private val values: Map[Dimension, String]) {
  def on = values.keys.toSet
  def defines(ds: Traversable[Dimension]) = ds.filterNot(on.contains).isEmpty

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
  val singluarity = new Point(Map.empty)
  def empty = singluarity
  def apply(d: Dimension, value: String) = new Point(Map(d -> value))
}