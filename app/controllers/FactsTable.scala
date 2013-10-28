package controllers

import play.api.mvc._
import support._
import cube._

object FactsTable extends Controller {

  def index(domainName: String) = DomainAction(domainName) { req ⇒
    req.dimensions.all.headOption.map { dim ⇒
      Redirect(routes.FactsTable.show(domainName, dim.name, Nil))
    }.getOrElse(NotFound)
  }

  def show(domainName: String, dimensionName: String, factNames: List[String], restrictTo: PointDefinition, invert: Boolean) = DimensionAction(domainName, dimensionName) { req ⇒
    val dimension = req.dimension
    val domain = req.fudimDomain
    val factOpts = factNames.distinct.map(domain.facts.get)
    if (factOpts.contains(None)) NotFound
    else {
      val facts = factOpts.map(_.get)
      val point = restrictTo(domain)
      val allDims: Set[Dimension] = req.dimensions.all.toSet
      val filterableDims = facts.map(_.dimensions).foldLeft(allDims - dimension)(_.intersect(_))
      if (!point.on.filterNot(filterableDims.contains).isEmpty) BadRequest
      else {
        HttpCache.cached(req, (domain.version +: facts.map(_.data.version(point))).max) {
          val otherFacts = (req.facts.all.toSet -- facts.toSet).filter(_.dimensions.contains(dimension))
          def linkFun(d: Dimension)(c: Option[Coordinate]) = {
            val p = c.fold(point - d)(v ⇒ point - d + v)
            routes.FactsTable.show(domainName, dimensionName, facts.map(_.name), p, invert).toString
          }
          Ok(views.html.factsTable(domain, dimension, filterableDims.toList.sortBy(_.name), point,
            facts, otherFacts.toList.sortBy(_.name), linkFun, invert))
        }
      }
    }
  }
}
