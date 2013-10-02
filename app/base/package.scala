import scala.util.continuations._

package object base {
  type Tx = Transaction[Unit]
  type <>[A] = Transaction[A]
  type <|> = Transaction[Unit]

  implicit class JoinableTransaction[A](t: Transaction[Transaction[A]]) {
    def join: Transaction[A] = t.flatMap(identity)
  }
  implicit class TraversableToTransaction[A](t: TraversableOnce[Transaction[A]]) {
    def sequence: Transaction[Seq[A]] = {
      t.foldLeft(Transaction.pure(List.empty[A])) { (sf, t) =>
        t.flatMap(e => sf.map(e :: _))
      }.map(_.reverse.toSeq)
    }
  }
  implicit class TraversableToTx(t: TraversableOnce[Tx]) {
    def sequence: Tx = t.foldLeft(Transaction.pure(()))((sf, t) => sf >> t)
  }

  /** Provide some methods on TraversableOne, so working with tx is easier. */
  implicit class TxTraversable[+A](l: TraversableOnce[A]) {
    def mapTx[B](f: A => B@tx): Seq[B]@tx = l.map(f(_).transaction).sequence.tx
  }
  /** Provide some methods on Option, so working with tx is easier. */
  implicit class TxOption[+A](o: Option[A]) {
    def mapTx[B](f: A => B@tx): Option[B]@tx = {
      if (o.isDefined) Some(f(o.get)).transaction
      else Transaction.pure(None)
    }.tx
  }


  /** Indicates that the value is the result of a transaction. To evaluate the value a transaction needs to be run. */
  type tx = cps[Transaction[Any]]

  /** Converts a transaction (monad) into a transactional value (continuation). The transaction is not executed. */
  def tx[A](transaction: Transaction[A]): A@tx = shift(transaction.flatMap)
  implicit def transactionToTx[A](transaction: Transaction[A]) = tx(transaction)
  implicit class TransactionToTx[A](transaction: Transaction[A]) {
    def tx = transactionToTx(transaction)
  }

  /** Converts an @tx value to the corresponding Transaction monad. */
  def transaction[A](body: => A@tx): Transaction[A] = {
    val ctx = reify[A, Transaction[Any], Transaction[Any]](body)
    val r = ctx.foreach(v => Transaction.pure(v))
    r.asInstanceOf[Transaction[A]]
  }
  implicit def txToTransaction[A](body: => A@tx) = transaction(body)
  implicit class TxToTransaction[A](body: => A@tx) {
    def transaction = txToTransaction(body)
  }

  /** Lifts the value into a @tx (equivalent to Transaction.pure) */
  def asTx[A](a: A): A@tx = Transaction.pure(a).tx
  implicit def valueToTx[A](a: A) = asTx(a)
  implicit class ValueToTx[A](a: A) {
    def tx = valueToTx(a)
  }

  /** Noop @tx value. Useful for things like 'if (cond) doTransaction else noop' */
  def noop = ().tx
  implicit def anyTxToUnitTx[A](a: => A@tx): Unit@tx = {
    a
    ()
  }
}
