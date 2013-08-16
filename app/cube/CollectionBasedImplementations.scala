package cube

/** Simple implementation of Dimension based on a list. */
case class ListDimension(name: String, data: List[String]) extends Dimension with CoordinateFactory {
  override lazy val values = data.zipWithIndex.map(v ⇒ (coordinate(this, v._2), v._1))
  def all: Seq[Coordinate] = values.map(_._1)
  def render(c: Coordinate): String = {
    values.find(_._1 == c).
      map(_._2).
      getOrElse(throw new IllegalArgumentException(s"Invalid coordinate $c for $name"))
  }
  def +(v: String) = copy(data = data :+ v)
  def -(v: String) = copy(data = data.filterNot(_ == v))

  def coordOf(v: String) = {
    values.find(_._2 == v).map(_._1).
      getOrElse(throw new IllegalArgumentException(s"Dimension $name does not contain $v"))
  }
}
object ListDimension {
  def apply(name: String, data: String*): ListDimension = ListDimension(name, data.toList)
}

/**
 * A immutable cube based on a Map.
 * Changed copies can be created using the +, ++, - and -- functions.
 */
class MapCube[D](
  protected override val allDimensions: Set[Dimension],
  private val data: Map[Point, D],
  override val slice: Point = Point.empty,
  protected override val filters: DimensionFilter = Map.empty) extends AbstractCube[D] {

  protected override type Self = MapCube[D]
  protected override def derive(slice: Point = slice, filters: DimensionFilter = filters) = {
    new MapCube(allDimensions, data, slice, filters)
  }

  override def get(at: Point) = Some(at).filter(matches).flatMap(data.get)
  override def sparse = data.filter(v ⇒ matches(v._1))
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

  override def equals(o: Any) = o match {
    case other: MapCube[_] ⇒ other.data == data && other.slice == slice && other.filters == filters
    case _ ⇒ false
  }
  override def hashCode = dimensions.hashCode ^ slice.hashCode ^ filters.hashCode
  override def toString = "MapCube(" + allDimensions.mkString(", ") + ") @ " + slice + " with " + filters
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
  /**
   * Create MapCube from the Point->value tuples.
   *  Note that all point must define exactly the same dimensions.
   */
  def apply[D](data: (Point, D)*): MapCube[D] = apply(data.toMap)
}