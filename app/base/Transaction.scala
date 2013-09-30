package base

/** Transaction monad. */
sealed trait Transaction[+A] {
  import Transaction._

  def run(context: TransactionState): A

  def map[B](f: A => B): Transaction[B] = flatMap(v => pure(f(v)))
  def flatMap[B](f: A => Transaction[B]): Transaction[B] = on[B] { context =>
    val a = run(context)
    f(a).run(context)
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
  def pure[A](value: => A) = on(_ => value)
  def noop = empty
  def empty: Tx = pure(())

  private[base] def on[A](f: TransactionState => A) = new Transaction[A] {
    override def run(context: TransactionState) = f(context)
  }
}

trait TransactionState

/** A ressource that can take part in a transaction. */
trait TransactionalRessource {
  /** Provides access to the transaction state. */
  protected def execute[A](f: TransactionState => A): Transaction[A] = Transaction.on(f)
}
