package base

import scala.util.control.Exception._

/** Transaction monad. */
sealed trait Transaction[+A] {
  import Transaction._

  def run(context: TransactionState): (TransactionState, Either[Exception, A])

  def map[B](f: A => B): Transaction[B] = flatMap(v => pure(f(v)))
  def flatMap[B](f: A => Transaction[B]): Transaction[B] = onEither[B] { state1 =>
    run(state1) match {
      case (state, Right(a)) => f(a).run(state)
      case (state, Left(e)) => (state, Left(e))
    }
  }

  def >>=[B](f: A => Transaction[B]) = flatMap(f)
  def >>[B](o: Transaction[B]) = flatMap(_ => o)
  def <*[B](o: Transaction[B]): Transaction[A] = {
    for {
      a <- this
      _ <- o
    } yield a
  }
  def *>[B](o: Transaction[B]) = this >> o
}
object Transaction {
  def apply[A](value: => A) = pure(value)
  def pure[A](value: => A) = on((_, value))
  def noop = empty
  def empty: Tx = pure(())

  private[base] def on[A](f: TransactionState => (TransactionState, A)) = onEither { state =>
    try {
      val (s2, r) = f(state)
      (s2, Right(r))
    } catch {
      case e: Exception => (state, Left(e))
    }
  }
  private[base] def onEither[A](f: TransactionState => (TransactionState, Either[Exception, A])) = new Transaction[A] {
    override def run(state: TransactionState) = f(state)
  }
}

trait TransactionState

/** A ressource that can take part in a transaction. */
trait TransactionalRessource {
  /** Provides access to the transaction state and allows to transform it. */
  protected def execute[A](f: TransactionState => (TransactionState, Either[Exception, A])): Transaction[A] = Transaction.onEither(f)
  /** Provides access to the transaction state. */
  protected def executeSafe[A](f: TransactionState => A): Transaction[A] = Transaction.on(s => (s, f(s)))
}
