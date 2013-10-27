package support

import scala.concurrent.Future
import play.api._
import play.api.mvc._
import models._
import models.playbinding.DomainRepo

class RequestWithDomain[A](val fudimDomain: Domain, request: Request[A]) extends WrappedRequest[A](request)
case class DomainAction(name: String) extends ActionBuilder[RequestWithDomain] {
  protected override def invokeBlock[A](request: Request[A], block: (RequestWithDomain[A]) ⇒ Future[SimpleResult]) = {
    DomainRepo.get(name).map(domain ⇒ block(new RequestWithDomain(domain, request))).
      getOrElse(Future.successful(Results.NotFound))
  }
}

class RequestWithDimension[A](val dimension: FudimDimension, fudimDomain: Domain, request: Request[A])
  extends RequestWithDomain[A](fudimDomain, request)
case class DimensionAction(domainName: String, name: String) extends ActionBuilder[RequestWithDimension] {
  protected override def invokeBlock[A](request: Request[A], block: (RequestWithDimension[A]) ⇒ Future[SimpleResult]) = {
    {
      for {
        domain ← DomainRepo.get(domainName)
        dimension ← domain.dimensionRepo.get(name)
      } yield block(new RequestWithDimension(dimension, domain, request))
    }.getOrElse(Future.successful(Results.NotFound))
  }
}

class RequestWithFact[A](val fact: Fact[_], fudimDomain: Domain, request: Request[A])
  extends RequestWithDomain[A](fudimDomain, request)
case class FactAction(domainName: String, name: String) extends ActionBuilder[RequestWithFact] {
  protected override def invokeBlock[A](request: Request[A], block: (RequestWithFact[A]) ⇒ Future[SimpleResult]) = {
    {
      for {
        domain ← DomainRepo.get(domainName)
        fact ← domain.factRepo.get(name)
      } yield block(new RequestWithFact(fact, domain, request))
    }.getOrElse(Future.successful(Results.NotFound))
  }
}
