package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import scalaz._
import Scalaz._
import base._
import cube._
import models._
import support._
import models.playbinding.Fudim

object Facts extends Controller {

  def list(domainName: String) = DomainAction(domainName) { req ⇒
    HttpCache.cached(req, req.fudimDomain.version) {
      Ok(views.html.facts(domainName, req.facts.all, FudimDataTypes.all, addForm))
    }
  }

  def add(domainName: String) = DomainAction(domainName) { implicit req ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(domainName, req.facts.all, FudimDataTypes.all, errors)),
      data ⇒ {
        val (name, dataTypeName) = data
        FudimDataTypes.get(dataTypeName).map { dataType ⇒
          Fudim.execTx {
            req.facts.createDatabaseBacked(name, dataType, Set.empty, Aggregation.none)
          }
          Redirect(routes.Facts.view(req.fudimDomain.name, name))
        }.getOrElse(BadRequest(s"Invalid data type $dataTypeName"))
      })
  }

  def view(domainName: String, name: String) = FactAction(domainName, name) { req ⇒
    val fact = req.fact
    HttpCache.cached(req, fact.version or req.fudimDomain.version) {
      val dims = req.dimensions.all.filterNot(fact.dimensions.contains)
      val aggr = Aggregation.unapply(fact.data).getOrElse(Aggregation.none)
      Ok(views.html.fact(domainName, fact, dims, fact.dataType.aggregations, aggrForm.fill(aggr.name)))
    }
  }
  def addDimension(domainName: String, factName: String, dimensionName: String) = modFactDim(domainName, factName, dimensionName) { (fact, moveTo) ⇒
    fact.addDimension(moveTo)
  }
  def removeDimension(domainName: String, factName: String, dimensionName: String) = modFactDim(domainName, factName, dimensionName) { (fact, keepAt) ⇒
    fact.removeDimension(keepAt)
  }
  private def modFactDim(domainName: String, factName: String, dimensionName: String)(f: (Fact[_], Coordinate) ⇒ Unit @tx) = FactAction(domainName, factName) { req ⇒
    val r = for {
      dimension ← req.dimensions.get(dimensionName).toSuccess(s"Dimension $dimensionName not found")
      coord ← dimension.all.headOption.toSuccess(s"Dimension $dimensionName has no values")
    } yield {
      Fudim.execTx(f(req.fact, coord))
      Redirect(routes.Facts.view(domainName, factName))
    }
    r.valueOr(e ⇒ NotFound(e))
  }

  def modifyDimension(domain: String, fact: String, dimension: String, action: String) = action match {
    case "PUT" ⇒ addDimension(domain, fact, dimension)
    case "DELETE" ⇒ removeDimension(domain, fact, dimension)
    case _ ⇒ Action(BadRequest(s"Unsupported Action $action"))
  }

  def setAggregation(domainName: String, factName: String) = FactAction(domainName, factName) { implicit req ⇒
    def changeAggr[T](fact: Fact[T], aggrName: String) = {
      val aggr = fact.dataType.aggregations.find(_.name == aggrName).getOrElse(Aggregation.none)
      Fudim.execTx {
        fact.aggregation = aggr
      }
      Redirect(routes.Facts.view(domainName, factName))
    }
    aggrForm.bindFromRequest.fold(
      errors ⇒ NotImplemented,
      aggrName ⇒ changeAggr(req.fact, aggrName))
  }

  def get(domainName: String, factName: String, at: PointDefinition) = FactAction(domainName, factName) { req ⇒
    val point = at(req.fact)
    HttpCache.cached(req, req.fact.data.version(point)) {
      req.fact.rendered.get(point).
        map(v ⇒ Ok(v)).
        getOrElse(NotFound)
    }
  }
  def save(domainName: String, factName: String, at: PointDefinition) = FactAction(domainName, factName) { req ⇒
    def setValue[T](fact: Fact[T], value: String) = fact.editor.map { editor ⇒
      val tpe = fact.dataType
      tpe.parse(value).map { v ⇒
        try {
          Fudim.execTx {
            editor.set(at(fact), v)
          }
          Ok(tpe.render(v))
        } catch {
          case ValueCannotBeSetException(_) ⇒ cannotSet
        }
      }.getOrElse(NotAcceptable("Value is not parsable"))
    }
    req.body.asText.filterNot(_.isEmpty).
      map(setValue(req.fact, _).getOrElse(cannotSet)).
      getOrElse(NotAcceptable)
  }
  private def cannotSet = MethodNotAllowed.withHeaders("Allow" -> "GET")

  val addForm = Form(tuple(
    "name" -> nonEmptyText,
    "type" -> nonEmptyText))
  val aggrForm = Form("aggregation" -> text)
}
