package cube

import models._
import scala.Option.option2Iterable

/** Data in a multi-dimensional space. */
trait Cube[+T] extends PartialFunction[Point, T] {
  protected type Self <: Cube[T]

  /** Value at the point. The point does not have to be fully defined, the cube might support aggregation of values (else None is returned). */
  def get(at: Point): Option[T]
  override def apply(at: Point) = get(at).get
  override def isDefinedAt(at: Point) = get(at).isDefined

  /** All points (along with their value) defined by the dimensions (within slice/dice). */
  def dense: Traversable[(Point, Option[T])]
  /** All points defined by the dimensions (within slice/dice), that have a value associated with them. */
  def sparse: Traversable[(Point, T)] = dense.flatMap(e ⇒ e._2.map((e._1, _)))
  /** All defined values at points within this cube (sliced/diced). */
  def values: Traversable[T] = sparse.map(_._2)

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

  def map[A](f: T ⇒ A): Cube[A] = MappedCube(this, f)
}

/** Cube that decorates another cube. I.e. to add aggragation of values. */
trait DecoratedCube[+T] extends Cube[T] {
  protected override type Self <: DecoratedCube[T]
  type Underlying <: Cube[T]
  val underlying: Underlying
}

trait AbstractDecoratedCube[+T] extends DecoratedCube[T] {
  protected def wrap(c: underlying.Self): Self
  override def get(at: Point) = underlying.get(at)
  override def dense = underlying.dense
  override def sparse = underlying.sparse
  override def slice = underlying.slice
  override def dimensions = underlying.dimensions
  override def raw = wrap(underlying.raw)
  override def slice(to: Point) = wrap(underlying.slice(to))
  override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
  override def toString = underlying.toString
}

object Cube {
  /** Removes all decoration from a cube. */
  def undecorate[T](cube: Cube[T]): Cube[T] = cube match {
    case c: DecoratedCube[T] ⇒ undecorate(c.underlying)
    case c ⇒ c
  }
}

/** Implements the slicing/dicing. */
trait AbstractCube[+T] extends Cube[T] {
  override protected type Self <: AbstractCube[T]

  override val slice: Point
  protected[this] val filters: DimensionFilter
  protected def allDimensions: Set[Dimension]

  override def dimensions = allDimensions -- slice.on
  override def raw = derive(Point.empty, Map.empty)
  override def slice(to: Point) = derive(slice = to)
  override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = {
    val combFilter = filters.get(dimension).map(f ⇒ (c: Coordinate) ⇒ f(c) && filter(c)).getOrElse(filter)
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

private object MappedCube {
  def apply[A, B](cube: Cube[A], f: A ⇒ B): Cube[B] = MappedCube(cube, f)

  case class MappedCube[A, B](cube: Cube[A], f: A ⇒ B) extends Cube[B] {
    override protected type Self = Cube[B]
    override def get(at: Point) = cube.get(at).map(f)
    override def dense = cube.dense.map(t ⇒ (t._1, t._2.map(f)))
    override def sparse = cube.sparse.map(t ⇒ (t._1, f(t._2)))
    override def values = cube.values.map(f)
    override def slice = cube.slice
    override def raw = wrap(cube.raw)
    override def dimensions = cube.dimensions
    override def slice(to: Point) = wrap(cube.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(cube.dice(dimension, filter))
    protected def wrap(cube: Cube[A]): Self = copy(cube = cube)
  }
}
