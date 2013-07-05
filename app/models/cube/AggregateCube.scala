package models.cube

import models._

/** Wrapper for a cube that calculates aggregated values for get that request points but lines (or even higher dimensional object). */
trait AggregateCube[D] extends DelegateCube[D] {
  protected val aggregator: Aggregator[D]

  override def get(at: Point) = {
    if (!underlying.slice.contains(at)) None // not contained in this cube, so ignore
    else {
      if (at.defines(dimensions)) underlying.get(at)
      else {
        val cube = underlying.slice(at)
        aggregator match {
          case a: SparseAggregator[D] ⇒ a(cube.values)
          case a: DenseAggregator[D] ⇒ a(cube.dense.map(_._2))
        }
      }
    }
  }
}
object AggregateCube {
  def apply[D](c: Cube[D], a: Aggregator[D]): AggregateCube[D] = NonEditableAggregateCube(c, a)
  def apply[D](c: EditableCube[D], a: Aggregator[D]): EditableCube[D] with AggregateCube[D] = EditableAggregateCube(c, a)

  private case class NonEditableAggregateCube[D](underlying: Cube[D], aggregator: Aggregator[D]) extends AggregateCube[D] {
    override protected type Underlying = Cube[D]
    override protected type Self = NonEditableAggregateCube[D]
    override def wrap(c: underlying.Self) = copy(underlying = c)
  }
  private case class EditableAggregateCube[D](underlying: EditableCube[D], aggregator: Aggregator[D]) extends AggregateCube[D] with DelegateEditableCube[D] {
    override protected type Underlying = EditableCube[D]
    override protected type Self = EditableAggregateCube[D]
    override def wrap(c: underlying.Self) = copy(underlying = c)
  }
}

/** Aggregates values. */
sealed trait Aggregator[D]
/** Aggregator that only cares for defined values. */
trait SparseAggregator[D] extends Aggregator[D] with Function1[Traversable[D], Option[D]] {
  def apply(v: Traversable[D]): Option[D]
}
/** Aggregator that cares for non-defined values. */
trait DenseAggregator[D] extends Aggregator[D] with Function1[Traversable[Option[D]], Option[D]] {
  def apply(v: Traversable[Option[D]]): Option[D]
}

object Aggregators {
  implicit def sparse[D](f: Traversable[D] ⇒ Option[D]) = new SparseAggregator[D] {
    override def apply(v: Traversable[D]) = f(v)
  }
  implicit def dense[D](f: Traversable[Option[D]] ⇒ Option[D]) = new DenseAggregator[D] {
    override def apply(v: Traversable[Option[D]]) = f(v)
  }

  def fold[A](initial: A)(f: (A, A) ⇒ A): Aggregator[A] = sparse(v ⇒ Some(v.fold(initial)(f)))
  def fold[A](initial: Option[A])(f: (Option[A], A) ⇒ Option[A]): Aggregator[A] = sparse(_.foldLeft(initial)(f))

  def reduce[A](f: (A, A) ⇒ A): Aggregator[A] = sparse(_.reduceOption(f))

  def sum = reduce[Long](_ + _)
}