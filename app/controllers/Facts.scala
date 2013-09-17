package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import cube._
import models._
import models.dbcube.DatabaseCube
import support.DomainAction
import support.FactAction
import support.PointDefinition

object Facts extends Controller {

  def list(domainName: String) = DomainAction(domainName) { domain ⇒
    Ok(views.html.facts(domainName, domain.factRepo.all, DataType.all, addForm))
  }

  def add(domainName: String) = DomainAction(domainName).on(domain ⇒ { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(domainName, domain.factRepo.all, DataType.all, errors)),
      data ⇒ {
        val (name, dataTypeName) = data
        DataType.get(dataTypeName).map { dataType =>
          val fact = domain.factRepo.createDatabaseBacked(name, dataType, Set.empty, None)
          Redirect(routes.Facts.view(domain.name, name))
        }.getOrElse(BadRequest(s"Invalid data type $dataTypeName"))
      })
  })

  def view(domainName: String, name: String) = DomainAction(domainName) { domain ⇒
    domain.factRepo.get(name).map { fact ⇒
      val dims = domain.dimensionRepo.all.filterNot(fact.dimensions.contains)
      val aggr = Aggregation.unapply(fact.data).getOrElse(Aggregation.none)
      Ok(views.html.fact(domainName, fact, dims, fact.dataType.aggregations, aggrForm.fill(aggr.name)))
    }.getOrElse(NotFound)
  }
  def addDimension(domainName: String, factName: String, dimensionName: String) = DomainAction(domainName) { domain ⇒
    val r = for {
      fact ← domain.factRepo.get(factName)
      dimension ← domain.dimensionRepo.get(dimensionName)
      moveTo ← dimension.all.headOption
    } yield {
      fact.addDimension(moveTo)
      Redirect(routes.Facts.view(domainName, factName))
    }
    r.getOrElse(NotFound)
  }
  def removeDimension(domainName: String, factName: String, dimensionName: String) = DomainAction(domainName) { domain ⇒
    val r = for {
      fact ← domain.factRepo.get(factName)
      dimension ← domain.dimensionRepo.get(dimensionName)
      keepAt ← dimension.all.headOption
    } yield {
      fact.removeDimension(keepAt)
      Redirect(routes.Facts.view(domainName, factName))
    }
    r.getOrElse(NotFound)
  }
  def modifyDimension(domain: String, fact: String, dimension: String, action: String) = action match {
    case "PUT" => addDimension(domain, fact, dimension)
    case "DELETE" => removeDimension(domain, fact, dimension)
    case _ => Action(BadRequest(s"Unsupported Action $action"))
  }

  def setAggregation(domainName: String, factName: String) = FactAction(domainName, factName).on(fact ⇒ { implicit request ⇒
    def changeAggr[T](fact: FudimFact[T], aggrName: String) = {
      val aggr = fact.dataType.aggregations.find(_.name == aggrName).getOrElse(Aggregation.none)
      fact.aggregation = aggr
      Redirect(routes.Facts.view(domainName, factName))
    }
    aggrForm.bindFromRequest.fold(
      errors ⇒ NotImplemented,
      aggrName ⇒ changeAggr(fact, aggrName))
  })

  def get(domainName: String, factName: String, at: PointDefinition) = FactAction(domainName, factName) { fact ⇒
    fact.rendered.get(at(fact)).map(v ⇒ Ok(v)).getOrElse(NotFound)
  }
  def save(domainName: String, factName: String, at: PointDefinition) = FactAction(domainName, factName).on(fact ⇒ { request ⇒
    def setValue[T](fact: FudimFact[T], value: String) = fact.data match {
      case cube: EditableCube[T] ⇒
        val tpe = fact.dataType
        tpe.parse(value).map { v ⇒
          try {
            cube.set(at(fact), v)
            Ok(tpe.render(v))
          } catch {
            case ValueCannotBeSetException(_) ⇒ cannotSet
          }
        }.getOrElse(NotAcceptable)
      case _ ⇒ cannotSet
    }
    request.body.asText.filterNot(_.isEmpty).
      map(setValue(fact, _)).
      getOrElse(NotAcceptable)
  })
  private def cannotSet = MethodNotAllowed.withHeaders("Allow" -> "GET")

  val addForm = Form(tuple(
    "name" -> nonEmptyText,
    "type" -> nonEmptyText
  ))
  val aggrForm = Form("aggregation" -> text)
}
