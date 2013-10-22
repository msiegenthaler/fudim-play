package base

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class TransactionSpec extends Specification {
  trait txs extends Scope {
    object Res extends TransactionalRessource {
      private[this] var state: Int = 0
      def value = state
      def get = executeSafe { _ ⇒ state }
      def set(v: Int) = executeSafe { _ ⇒
        state = v
        v
      }
      def increment = executeSafe { _ ⇒
        val nv = state + 1
        state = nv
        nv
      }
      def setStateToThis = execute {
        case Txs(v) ⇒ (Txs(state), Right(()))
      }
      def setThisToState = execute {
        case Txs(v) ⇒
          state = v
          (Txs(v), Right(()))
      }
      def throwException = executeSafe { _ ⇒ throw MyException() }
      def throwException2 = execute { _ ⇒ (Txs(666), Left(MyException())) }
    }
    case class MyException() extends Exception

    case class Txs(value: Int = 0) extends TransactionState

    def run[A](tx: Transaction[A], initialValue: Int = 0) = {
      val (s, r) = tx.run(Txs(initialValue))
      (s, r.fold(throw _, identity))
    }

    def assertionAction(p: Txs ⇒ Unit): Transaction[Unit] = Transaction.on {
      case t: Txs ⇒ p(t); (t, ())
    }
  }

  "Transaction" should {
    "catch exceptions that occur in ressources.executeSafe and return them as Left()" in new txs {
      val tx = Res.throwException
      val (s, v) = tx.run(Txs())
      v must_== Left(MyException())
      s must_== Txs()
    }
    "catch exceptions that occur in ressources.execute and return them as Left()" in new txs {
      val tx = Res.throwException2
      val (s, v) = tx.run(Txs())
      v must_== Left(MyException())
      s must_== Txs(666)
    }
    "be executable more than once" in new txs {
      val tx = Res.increment >> Res.increment
      val (s1, r1) = run(tx)
      val (s2, r2) = run(tx)
      r1 must_== 2
      r2 must_== 4 //because it depends on resource state
    }
    "be executable more than once with seperate state" in new txs {
      val tx = Res.setThisToState >> Res.increment >> Res.increment
      val (s1, r1) = run(tx)
      val (s2, r2) = run(tx)
      r1 must_== 2
      r2 must_== 2
    }
  }

  "Transaction.pure" should {
    "return the unchanged state in run" in new txs {
      val a = Transaction.pure(10)
      val (s1, _) = run(a, 10)
      s1 must_== Txs(10)
      val (s2, _) = run(a, 12)
      s2 must_== Txs(12)
    }
    "return the value to run" in new txs {
      List(10, 0, "Hallo", ()).foreach { v ⇒
        val a = Transaction.pure(v)
        val (_, v1) = run(a, 1)
        v1 must_== v
        val (_, v2) = run(a, 2)
        v2 must_== v
      }
    }
    "catch exceptions that occur in the bodies and return them as Left()" in new txs {
      val tx = Transaction.pure(throw MyException())
      val (s, v) = tx.run(Txs())
      v must_== Left(MyException())
    }
  }

  "Transaction.flatMap" should {
    "execute the first action" in new txs {
      val tx = Res.set(3) >> Res.get
      val (_, v) = run(tx)
      v must_== 3
      Res.value must_== 3
    }
    "execute the second action" in new txs {
      val tx = Res.increment >> Res.set(10)
      val (_, v) = run(tx)
      v must_== 10
      Res.value must_== 10
    }
    "execute both actions" in new txs {
      val tx = Res.increment >> Res.increment
      val (_, v) = run(tx)
      v must_== 2
      Res.value must_== 2
    }
    "pass the result of the first action to the second" in new txs {
      def addTwoAndSet(to: Int) = Res.set(to + 2)
      val tx = Transaction.pure(111) >>= addTwoAndSet
      val (_, v) = run(tx)
      v must_== 113
      Res.value must_== 113
    }
    "pass the txstate of the first action to the second" in new txs {
      val tx = Res.set(123) >> Res.setStateToThis >> Res.set(10)
      val (Txs(tv), v) = run(tx)
      tv must_== 123
      v must_== 10
    }

    "short circuit on 'safe' exception (not execute the following Txs)" in new txs {
      val tx = Res.set(6) >> Res.throwException >> Res.increment
      val (_, v) = tx.run(Txs())
      v must_== Left(MyException())
      Res.value must_== 6
    }
    "short circuit on exception (not execute the following Txs)" in new txs {
      val tx = Res.set(6) >> Res.throwException2 >> Res.increment
      val (_, v) = tx.run(Txs())
      v must_== Left(MyException())
      Res.value must_== 6
    }
    "return the state before the exception on safe ressources" in new txs {
      val tx = Res.set(6) >> Res.setStateToThis >> Res.throwException >> Res.increment >> Res.setStateToThis
      val (Txs(s), _) = tx.run(Txs())
      s must_== 6

      val tx2 = Res.set(2) >> Res.increment >> Res.setStateToThis >> Res.throwException >> Res.increment >> Res.setStateToThis
      val (Txs(s2), _) = tx2.run(Txs())
      s2 must_== 3
    }
    "return the exception-state on ressources.execute" in new txs {
      val tx = Res.set(6) >> Res.setStateToThis >> Res.throwException2 >> Res.increment >> Res.setStateToThis
      val (Txs(s), _) = tx.run(Txs())
      s must_== 666

      val tx2 = Res.set(2) >> Res.increment >> Res.setStateToThis >> Res.throwException2 >> Res.increment >> Res.setStateToThis
      val (Txs(s2), _) = tx2.run(Txs())
      s2 must_== 666
    }
  }

  "Transaction.<*" should {
    "execute both actions" in new txs {
      val tx = Res.increment <* Res.increment
      run(tx)
      Res.value must_== 2
    }
    "return the result of the first action" in new txs {
      val tx = Res.set(10) <* Res.increment
      val (_, v) = run(tx)
      v must_== 10
      Res.value must_== 11
    }
  }
  "Transaction.*>" should {
    "execute both actions" in new txs {
      val tx = Res.increment *> Res.increment
      run(tx)
      Res.value must_== 2
    }
    "return the result of the second action" in new txs {
      val tx = Res.set(10) *> Res.increment
      val (_, v) = run(tx)
      v must_== 11
      Res.value must_== 11
    }
  }
}
