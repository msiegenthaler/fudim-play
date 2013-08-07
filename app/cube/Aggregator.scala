package cube

import models._
import support.JsonMapper
import support.JsonMapperRepository

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
  def apply[D](aggregator: Aggregator[D]) = decorator(aggregator)
  def unapply[D](dec: CubeDecorator[D]): Option[Aggregator[D]] = dec match {
    case SparseAggregationDecorator(aggr) ⇒ Some(aggr)
    case DenseAggregationDecorator(aggr) ⇒ Some(aggr)
    case _ ⇒ None
  }

  implicit def sparse[D](f: Traversable[D] ⇒ Option[D]) = new SparseAggregator[D] {
    override def apply(v: Traversable[D]) = f(v)
  }
  implicit def dense[D](f: Traversable[Option[D]] ⇒ Option[D]) = new DenseAggregator[D] {
    override def apply(v: Traversable[Option[D]]) = f(v)
  }

  def json(aggrRepo: JsonMapperRepository[Aggregator[_]]) = new JsonMapper[CubeDecorator[_]] {
    val id = "aggregator"
    override def parser = json ⇒ {
      aggrRepo.parse(json).map(decorator(_))
    }
    override def serializer = {
      case DenseAggregationDecorator(aggregator) ⇒
        aggrRepo.serialize(aggregator)
      case SparseAggregationDecorator(aggregator) ⇒
        aggrRepo.serialize(aggregator)
    }
  }

  private case class DenseAggregationDecorator[D](aggregator: DenseAggregator[D]) extends CubeDecorator[D] {
    override def get(decoratee: Cube[D])(at: Point) = {
      if (!decoratee.slice.contains(at)) None // not contained in this cube, so ignore
      else if (at.defines(decoratee.dimensions)) decoratee.get(at)
      else aggregator(decoratee.slice(at).dense.map(_._2))
    }
    override def toString = s"DenseAggregator($aggregator)"
  }
  private case class SparseAggregationDecorator[D](aggregator: SparseAggregator[D]) extends CubeDecorator[D] {
    override def get(decoratee: Cube[D])(at: Point) = {
      if (!decoratee.slice.contains(at)) None // not contained in this cube, so ignore
      else if (at.defines(decoratee.dimensions)) decoratee.get(at)
      else aggregator(decoratee.slice(at).values)
    }
    override def toString = s"SparseAggregator($aggregator)"
  }
}

object Aggregators {
  import Aggregator._
  def fold[A](initial: A)(f: (A, A) ⇒ A): Aggregator[A] = sparse(v ⇒ Some(v.fold(initial)(f)))
  def fold[A](initial: Option[A])(f: (Option[A], A) ⇒ Option[A]): Aggregator[A] = sparse(_.foldLeft(initial)(f))

  def reduce[A](f: (A, A) ⇒ A): Aggregator[A] = sparse(_.reduceOption(f))

  def sum = reduce[Long](_ + _)
}
