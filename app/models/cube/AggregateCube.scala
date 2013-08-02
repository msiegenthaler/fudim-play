package models.cube

import models._

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

object Aggregator {
  /** Creates a  CubeDecorator from the aggregator. */
  implicit def decorator[D](aggregator: Aggregator[D]): CubeDecorator[D] = aggregator match {
    case a: SparseAggregator[D] ⇒ new SparseAggregationDecorator(a)
    case a: DenseAggregator[D] ⇒ new DenseAggregationDecorator(a)
  }

  implicit def sparse[D](f: Traversable[D] ⇒ Option[D]) = new SparseAggregator[D] {
    override def apply(v: Traversable[D]) = f(v)
  }
  implicit def dense[D](f: Traversable[Option[D]] ⇒ Option[D]) = new DenseAggregator[D] {
    override def apply(v: Traversable[Option[D]]) = f(v)
  }

  private case class DenseAggregationDecorator[D](aggregator: DenseAggregator[D]) extends CubeDecorator[D] {
    override def get(decoratee: Cube[D])(at: Point) = {
      if (!decoratee.slice.contains(at)) None // not contained in this cube, so ignore
      else if (at.defines(decoratee.dimensions)) decoratee.get(at)
      else aggregator(decoratee.slice(at).dense.map(_._2))
    }
  }
  private case class SparseAggregationDecorator[D](aggregator: SparseAggregator[D]) extends CubeDecorator[D] {
    override def get(decoratee: Cube[D])(at: Point) = {
      if (!decoratee.slice.contains(at)) None // not contained in this cube, so ignore
      else if (at.defines(decoratee.dimensions)) decoratee.get(at)
      else aggregator(decoratee.slice(at).values)
    }
  }
}

object Aggregators {
  import Aggregator._
  def fold[A](initial: A)(f: (A, A) ⇒ A): Aggregator[A] = sparse(v ⇒ Some(v.fold(initial)(f)))
  def fold[A](initial: Option[A])(f: (Option[A], A) ⇒ Option[A]): Aggregator[A] = sparse(_.foldLeft(initial)(f))

  def reduce[A](f: (A, A) ⇒ A): Aggregator[A] = sparse(_.reduceOption(f))

  def sum = reduce[Long](_ + _)
}
