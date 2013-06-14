package models

/** Data in a multi dimensional space. */
trait CubeData[D] extends PartialFunction[Point, D] {
  /** Value at the fully defined point. Restricted to the slice and dice. */
  def get(at: Point): Option[D] = slice(at).dense.headOption.flatMap(_._2)
  def apply(at: Point) = get(at).get
  def isDefinedAt(at: Point) = get(at).isDefined

  /** All values in this cube. This is a sparse view, points without values are not in the traversable. */
  def values: Traversable[D] = sparse.map(_._2)
  /** All points with a value in this cube. Only the points with a value in it are returned. */
  def sparse: Traversable[(Point, D)] = dense.flatMap(e ⇒ e._2.map((e._1, _)))
  /** All possible values as defined by the dimensions (within slice/dice). */
  def dense: Traversable[(Point, Option[D])]

  /** This cube without any slicing and dicing applied. 'this' if the data is not sliced/diced. */
  def raw: CubeData[D]

  /** The fully fixed dimension values, in other words the slice. */
  def slice: Point
  /** Fix the dimensions of the point. The point must be fully defined, so use slice + (d, value) if you want to add the d=value condition. */
  def slice(to: Point): CubeData[D]
  /** The 'free' dimensions. Dimensions that are not 'locked down' (sliced). Filtered (diced) dimensions are included. */
  def dimensions: Set[Dimension] = raw.dimensions -- slice.on

  /** Restrict the values within a dimension. If the dimension is already filtered then the both filters are combined with AND. */
  def dice(dimension: Dimension, filter: String ⇒ Boolean): CubeData[D]
}
object CubeData {
  type DimensionFilter = Map[Dimension, String ⇒ Boolean]
}

import CubeData._

/** Implements the slicing/dicing. */
trait AbstractCubeData[D] extends CubeData[D] {
  protected type self <: CubeData[D]

  val slice: Point
  protected[this] val filters: DimensionFilter

  def raw = derive(Point.empty, Map.empty)
  def slice(to: Point) = derive(slice = to)
  def dice(dimension: Dimension, filter: String ⇒ Boolean) = {
    val combFilter = filters.get(dimension).map(f ⇒ ((v: String) ⇒ f(v) && filter(v))).getOrElse(filter)
    derive(filters = filters + (dimension -> combFilter))
  }
  protected def derive(slice: Point = slice, filters: DimensionFilter = filters): self
}