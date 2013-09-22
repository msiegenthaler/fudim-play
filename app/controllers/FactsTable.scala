package controllers

import play.api.mvc._
import support._
import cube._

object FactsTable extends Controller {

  def index(domainName: String) = DomainAction(domainName) { domain =>
    domain.dimensions.headOption.map { dim =>
      Redirect(routes.FactsTable.show(domainName, dim.name, Nil))
    }.getOrElse(NotFound)
  }

  def show(domainName: String, dimensionName: String, factNames: List[String], restrictTo: PointDefinition = PointDefinition.empty) = DomainAction(domainName) { domain =>
    domain.dimensionRepo.get(dimensionName).map { dimension =>
      val factOpts = factNames.distinct.map(domain.factRepo.get)
      if (factOpts.contains(None)) NotFound
      else {
        val facts = factOpts.map(_.get)
        val otherFacts = (domain.factRepo.all.toSet -- facts.toSet).filter(_.dimensions.contains(dimension))
        val filterableDims = facts.map(_.dimensions).foldLeft(domain.dimensions - dimension)(_.intersect(_))
        val point = restrictTo(domain)
        if (!point.on.filterNot(filterableDims.contains).isEmpty) BadRequest
        else {
          def linkFun(d: Dimension)(c: Option[Coordinate]) = {
            val p = c.fold(point - d)(v => point - d + v)
            routes.FactsTable.show(domainName, dimensionName, facts.map(_.name), p).toString
          }
          Ok(views.html.factsTable(domain, dimension, filterableDims.toList.sortBy(_.name), point,
            facts, otherFacts.toList.sortBy(_.name), linkFun))
        }
      }
    }.getOrElse(NotFound)
  }
}
