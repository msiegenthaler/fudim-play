package controllers

import play.api.mvc._
import support.DomainAction

object FactsTable extends Controller {

  def index(domainName: String) = DomainAction(domainName) { domain =>
    domain.dimensions.headOption.map { dim =>
      Redirect(routes.FactsTable.show(domainName, dim.name, Nil))
    }.getOrElse(NotFound)
  }

  def show(domainName: String, dimensionName: String, factNames: List[String]) = DomainAction(domainName) { domain =>
    domain.dimensionRepo.get(dimensionName).map { dimension =>
      val factOpts = factNames.map(domain.factRepo.get)
      if (factOpts.contains(None)) NotFound
      else {
        val facts = factOpts.map(_.get)
        val dataFuns = facts.map(f => f.rendered.get _)
        Ok(views.html.factsTable(domain, dimension, facts, dataFuns))
      }
    }.getOrElse(NotFound)
  }
}
