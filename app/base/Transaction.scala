package base

/** Transaction monad. */
sealed trait Transaction[A] {
  import Transaction._

  def run(context: TransactionState): A

  def map[B](f: A => B): Transaction[B] = flatMap(f.andThen(pure))
  def flatMap[B](f: A => Transaction[B]): Transaction[B] = on[B] { context =>
    val a = run(context)
    f(a).run(context)
  }

  def >>=[B](f: A => Transaction[B]) = flatMap(f)
  def >>[B](o: Transaction[B]) = flatMap(_ => o)
}
object Transaction {
  def apply[A](value: A) = pure(value)
  def pure[A](value: A) = on(_ => value)
  private[base] def on[A](f: TransactionState => A) = new Transaction[A] {
    override def run(context: TransactionState) = f(context)
  }

  implicit class JoinableTransaction[A](t: Transaction[Transaction[A]]) {
    def join: Transaction[A] = t.flatMap(identity)
  }
  implicit class TraversableToTransaction[A](t: TraversableOnce[Transaction[A]]) {
    def sequence: Transaction[Seq[A]] = {
      t.foldLeft(pure(List.empty[A])) { (sf, t) =>
        t.flatMap(e => sf.map(e :: _))
      }.map(_.reverse.toSeq)
    }
  }
}

trait TransactionState

/** A ressource that can take part in a transaction. */
trait TransactionalRessource[T <: TransactionState] {
  /** Provides access to the transaction state. */
  protected def execute[A](f: T => A): Transaction[A] = Transaction.on {
    case t: T => f(t)
    case _ => throw new IllegalStateException("Transaction does not support ressource type " + classOf[T].getName)
  }
}
