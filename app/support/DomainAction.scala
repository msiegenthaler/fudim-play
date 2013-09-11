package support

import play.api._
import play.api.mvc._
import models._
import models.play.DomainRepo

trait ObjectAction[T] {
  def apply(f: T ⇒ Result): Action[AnyContent]
  def on(f: T ⇒ Request[_] ⇒ Result): Action[AnyContent]
}
trait ObjectActionCompanion[I, T] {
  protected def get(id: I): Option[T]
  private def in(id: I)(f: T ⇒ Result): Result = {
    get(id).map(f).getOrElse(Results.NotFound)
  }
  def apply(id: I) = new ObjectAction[T] {
    def apply(f: T ⇒ Result) = Action(in(id)(f))
    def on(f: (Request[_], T) ⇒ Result) = {
      Action(request ⇒ in(id)(obj ⇒ f(request, obj)))
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