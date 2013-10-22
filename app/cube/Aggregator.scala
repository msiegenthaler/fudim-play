package cube

import models._
import support.JsonMapper
import support.JsonMapperRepository

/** Aggregates values. */
sealed trait Aggregator[T]
/** Aggregator that only cares for defined values. */
trait SparseAggregator[T] extends Aggregator[T] with (Traversable[T] ⇒ Option[T]) {
  def apply(v: Traversable[T]): Option[T]
}
/** Aggregator that cares for non-defined values. */
trait DenseAggregator[T] extends Aggregator[T] with (Traversable[Option[T]] ⇒ Option[T]) {
  def apply(v: Traversable[Option[T]]): Option[T]
}

object Aggregator {
  /** Creates a  CubeDecorator from the aggregator. */
  implicit def decorator[T](aggregator: Aggregator[T]): CubeDecorator[T] = aggregator match {
    case a: SparseAggregator[T] ⇒ new SparseAggregationDecorator(a)
    case a: DenseAggregator[T] ⇒ new DenseAggregationDecorator(a)
  }
  def apply[T](aggregator: Aggregator[T]) = decorator(aggregator)
  def unapply[T](dec: CubeDecorator[T]): Option[Aggregator[T]] = dec match {
    case SparseAggregationDecorator(aggr) ⇒ Some(aggr)
    case DenseAggregationDecorator(aggr) ⇒ Some(aggr)
    case _ ⇒ None
  }

  implicit def sparse[T](f: Traversable[T] ⇒ Option[T]) = new SparseAggregator[T] {
    override def apply(v: Traversable[T]) = f(v)
  }
  implicit def dense[T](f: Traversable[Option[T]] ⇒ Option[T]) = new DenseAggregator[T] {
    override def apply(v: Traversable[Option[T]]) = f(v)
  }

  def json(aggrRepo: JsonAggregatorMapperRepository) = new JsonCubeDecoratorMapper {
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

  private case class DenseAggregationDecorator[T](aggregator: DenseAggregator[T]) extends CubeDecorator[T] {
    override def get(decoratee: Cube[T])(at: Point) = {
      if (!decoratee.slice.contains(at)) None // not contained in this cube, so ignore
      else if (at.defines(decoratee.dimensions)) decoratee.get(at)
      else aggregator(decoratee.slice(at).dense.map(_._2))
    }
    override def toString = s"DenseAggregator($aggregator)"
  }
  private case class SparseAggregationDecorator[T](aggregator: SparseAggregator[T]) extends CubeDecorator[T] {
    override def get(decoratee: Cube[T])(at: Point) = {
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

  def sumLong = reduce[Long](_ + _)
  def sumInt = reduce[Int](_ + _)

  def avgLong = sparse[Long] { vs ⇒
    val (count, sum) = vs.foldLeft((0L, 0L))((s, v) ⇒ (s._1 + 1, s._2 + v))
    Some((sum.toDouble / count).round)
  }
  def avgInt = sparse[Int] { vs ⇒
    val (count, sum) = vs.foldLeft((0, 0))((s, v) ⇒ (s._1 + 1, s._2 + v))
    Some((sum.toDouble / count).round.toInt)
  }
}
