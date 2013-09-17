package support

import play.api._
import play.api.mvc._
import models._
import models.playbinding.DomainRepo

trait ObjectAction[T] {
  def apply(f: T ⇒ Result): Action[AnyContent]
  def on(f: T ⇒ Request[AnyContent] ⇒ Result): Action[AnyContent]
}
trait ObjectActionCompanion[I, T] {
  protected def get(id: I): Option[T]
  private def in(id: I)(f: T ⇒ Result): Result = {
    get(id).map(f).getOrElse(Results.NotFound)
  }
  def apply(id: I) = new ObjectAction[T] {
    override def apply(f: T ⇒ Result) = Action(in(id)(f))
    override def on(f: T ⇒ Request[AnyContent] ⇒ Result): Action[AnyContent] = {
      Action(request ⇒ in(id)(obj ⇒ f(obj)(request)))
    }
  }
}

object DomainAction extends ObjectActionCompanion[String, FudimDomain] {
  override protected def get(id: String) = DomainRepo.get(id)
}

object DimensionAction extends ObjectActionCompanion[(String, String), FudimDimension] {
  override protected def get(id: (String, String)) = {
    val (domain, dimension) = id
    DomainRepo.get(domain).flatMap(_.dimensionRepo.get(dimension))
  }
}

object FactAction extends ObjectActionCompanion[(String, String), FudimFact[_]] {
  override protected def get(id: (String, String)) = {
    val (domain, fact) = id
    DomainRepo.get(domain).flatMap(_.factRepo.get(fact))
  }
}