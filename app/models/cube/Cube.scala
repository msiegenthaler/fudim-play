package models.cube

import models._
import scala.Option.option2Iterable

/** Data in a multi-dimensional space. */
trait Cube[D] extends PartialFunction[Point, D] {
  protected type Self <: Cube[D]

  /** Value at the point. The point does not have to be fully defined, the cube might support aggregation of values (else None is returned). */
  def get(at: Point): Option[D]
  override def apply(at: Point) = get(at).get
  override def isDefinedAt(at: Point) = get(at).isDefined

  /** All points (along with their value) defined by the dimensions (within slice/dice). */
  def dense: Traversable[(Point, Option[D])]
  /** All points defined by the dimensions (within slice/dice), that have a value associated with them. */
  def sparse: Traversable[(Point, D)] = dense.flatMap(e ⇒ e._2.map((e._1, _)))
  /** All defined values at points within this cube (sliced/diced). */
  def values: Traversable[D] = sparse.map(_._2)

  /** This cube without any slicing and dicing applied. */
  def raw: Self

  /** The fixed dimension values, in other words the slice. */
  def slice: Point
  /** Fix the dimensions as defined by the point. The point is absolute, so use slice + (d, value) if you want to add the d=value condition. */
  def slice(to: Point): Self
  /** The 'free' dimensions. Dimensions that are not 'locked down' (sliced). */
  def dimensions: Set[Dimension]

  /** Restrict the values within a dimension. If the dimension is already filtered then the both filters are combined with AND. */
  def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean): Self
}

/** Cube that decorates another cube. I.e. to add aggragation of values. */
trait DecoratedCube[D] extends Cube[D] {
  protected override type Self <: DecoratedCube[D]
  type Underlying <: Cube[D]
  val underlying: Underlying
}

object Cube {
  type DimensionFilter = Map[Dimension, Coordinate ⇒ Boolean]

  /** Removes all decoration from a cube. */
  def undecorate[D](cube: Cube[D]): Cube[D] = cube match {
    case c: DecoratedCube[D] ⇒ undecorate(c.underlying)
    case c ⇒ c
  }

  /** see EditableCube.from */
  def editable[D](cube: Cube[D]) = EditableCube.from(cube)
}

/** Editable date in a multi-dimensional space. */
trait EditableCube[D] extends Cube[D] {
  protected type Self <: EditableCube[D]

  /** Whether the value at this point can be set. */
  def isSettable(at: Point): Boolean
  /** Set the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def set(at: Point, value: Option[D]): Unit
  /** Set the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def set(at: Point, value: D): Unit = set(at, Some(value))
  /** Remove the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def remove(at: Point) = set(at, None)

  /** Set data in this cube. Slice/dice does apply (non-matching are not changed). */
  def setAll(value: Option[D]): Unit
  /** Remove all data in this cube. Slice/dice does apply (non-matching are not deleted). */
  def clear = setAll(None)
}
object EditableCube {
  /**
   * If the cube is editable it is returned casted.
   * If it is not editable it is wrapped with a cube that has isSettable=false for all cells. The wrapped thing is a DecoratedCube.
   */
  def from[D](cube: Cube[D]): EditableCube[D] = cube match {
    case cube: EditableCube[D] ⇒ cube
    case cube ⇒ new PseudoEditableCube(cube)
  }

  private class PseudoEditableCube[D](val underlying: Cube[D]) extends EditableCube[D] with DecoratedCube[D] {
    override protected type Self = PseudoEditableCube[D]
    override type Underlying = Cube[D]
    private def wrap(c: Cube[D]) = new PseudoEditableCube(c)

    override def get(at: Point) = underlying.get(at)
    override def dense = underlying.dense
    override def sparse = underlying.sparse
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
    override def isSettable(at: Point) = false
    override def set(at: Point, value: Option[D]) = throw ValueCannotBeSetException(at)
    override def setAll(value: Option[D]) = ()
  }
}
case class ValueCannotBeSetException(at: Point) extends RuntimeException(s"Cannot set value at $at")

/** Implements the slicing/dicing. */
trait AbstractCube[D] extends Cube[D] {
  import Cube._
  protected type Self <: AbstractCube[D]

  val slice: Point
  protected[this] val filters: DimensionFilter
  protected def allDimensions: Set[Dimension]

  override def dimensions = allDimensions -- slice.on
  def raw = derive(Point.empty, Map.empty)
  def slice(to: Point) = derive(slice = to)
  def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = {
    val combFilter = filters.get(dimension).map(f ⇒ ((c: Coordinate) ⇒ f(c) && filter(c))).getOrElse(filter)
    derive(filters = filters + (dimension -> combFilter))
  }
  protected def derive(slice: Point = slice, filters: DimensionFilter = filters): Self

  /** If this point is inside this cube. */
  protected def matches(p: Point): Boolean = {
    slice.contains(p) &&
      (p -- slice.on).coordinates.forall(e ⇒ filters.get(e.dimension).map(f ⇒ f(e)).getOrElse(true))
  }
  /** All points in this cube. */
  protected def allPoints: Traversable[Point] = {
    dimensions.foldLeft(Seq(slice)) { (ps, d) ⇒
      val coords = filters.get(d) match {
        case Some(f) ⇒ d.all.filter(f)
        case None ⇒ d.all
      }
      ps.flatMap(p ⇒ coords.map(c ⇒ p + c))
    }
  }
}