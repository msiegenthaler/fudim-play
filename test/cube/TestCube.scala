package cube

import Cube._

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

/**
 * A immutable cube based on a Map.
 * Changed copies can be created using the +, ++, - and -- functions.
 */
class MapCube[D](
  protected override val allDimensions: Set[Dimension],
  data: Map[Point, D],
  override val slice: Point = Point.empty,
  protected override val filters: DimensionFilter = Map.empty) extends AbstractCube[D] {

  protected override type Self = MapCube[D]
  protected override def derive(slice: Point = slice, filters: DimensionFilter = filters) = {
    new MapCube(allDimensions, data, slice, filters)
  }

  override def get(at: Point) = data.get(at)
  override def sparse = data
  override def dense = allPoints.map(p ⇒ (p, get(p)))

  def +(kv: (Point, D)) = {
    if (!kv._1.definesExactly(allDimensions))
      throw new IllegalArgumentException(s"Point ${kv._1} is invalid, does not match dimensions of cube: ${dimensions.mkString(",")}")
    new MapCube(allDimensions, data + kv)
  }
  def ++(ds: Traversable[(Point, D)]) = {
    ds.map(_._1).filterNot(_.definesExactly(allDimensions)).
      foreach(p ⇒ throw new IllegalArgumentException(s"Point $p is invalid, does not match dimensions of cube: ${dimensions.mkString(",")}"))
    new MapCube(dimensions, data ++ ds)

  }
  def -(p: Point) = new MapCube(allDimensions, data - p)
  def --(p: Traversable[Point]) = new MapCube(allDimensions, data -- p)
}
object MapCube {
  /** An empty MapCube. */
  def apply[D](dimensions: Traversable[Dimension]) = new MapCube[D](dimensions.toSet, Map.empty)
  /**
   * Create a MapCube from a Map containing the data.
   *  Note that all point must define exactly the same dimensions. If the map is empty then a zero-dimensional cube will be created.
   */
  def apply[D](data: Map[Point, D]) = {
    val dimensions = data.keys.headOption.map(_.on).getOrElse(Set.empty)
    data.keys.filterNot(_.definesExactly(dimensions)).
      foreach(p ⇒ throw new IllegalArgumentException(s"Point $p is invalid, does not match dimensions of cube: ${dimensions.mkString(",")}"))
    new MapCube(dimensions, data)
  }
}