package support

import org.specs2.mutable.BeforeAfter
import org.specs2.specification.Scope
import play.api.Play
import play.api.test.FakeApplication
import base._
import models.playbinding.{Fudim, FudimResources}


/** Base trait for scopes that use the model. */
trait withModel extends Scope with BeforeAfter {
  override def before = Play.start(FakeApplication())
  override def after = Play.stop

  def exec[A](tx: Transaction[A]): A = Fudim.exec(tx)
  def execTx[A](b: => A@tx): A = Fudim.execTx(b)
}
