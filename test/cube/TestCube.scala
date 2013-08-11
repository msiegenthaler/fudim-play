package cube

/** Simple implementation of Dimension based on a list. */
case class ListDimension private (name: String, data: List[String]) extends Dimension with CoordinateFactory {
  override lazy val values = data.zipWithIndex.map(v ⇒ (coordinate(this, v._2), v._1))
  def all: Seq[Coordinate] = values.map(_._1)
  def render(c: Coordinate): String = {
    values.find(_._1 == c).
      map(_._2).
      getOrElse(throw new IllegalArgumentException(s"Invalid coordinate $c for $name"))
  }
  def +(v: String) = copy(data = data :+ v)
}