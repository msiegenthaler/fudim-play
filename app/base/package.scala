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
}
