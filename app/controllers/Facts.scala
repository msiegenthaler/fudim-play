package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import cube._
import models._
import models.dbcube.DatabaseCube

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(Fact.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(Fact.all, errors)),
      name ⇒ {
        val fact = Fact.createDatabaseBacked(name, Set.empty, None)
        Redirect(routes.Facts.view(name))
      })
  }

  def view(name: String) = Action {
    Fact.get(name).map { fact ⇒
      val dims = DimensionRepo.all.filterNot(fact.dimensions.contains)
      val aggr = Aggregation.unapply(fact.cube).getOrElse(Aggregation.none)
      Ok(views.html.fact(fact, dims, Aggregation.all, aggrForm.fill(aggr.name)))
    }.getOrElse(NotFound)
  }
  def addDimension(factName: String, dimensionName: String) = Action {
    val r = for {
      fact ← Fact.get(factName)
      dimension ← DimensionRepo.get(dimensionName)
      moveTo ← dimension.all.headOption
    } yield {
      fact.addDimension(moveTo)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    val r = for {
      fact ← Fact.get(factName)
      dimension ← DimensionRepo.get(dimensionName)
      keepAt ← dimension.all.headOption
    } yield {
      fact.removeDimension(keepAt)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }

  def setAggregation(factName: String) = Action { implicit request ⇒
    aggrForm.bindFromRequest.fold(
      errors ⇒
        NotImplemented,
      aggrName ⇒ {
        Fact.get(factName).map { fact ⇒
          val aggr = Aggregation.all.find(_.name == aggrName).getOrElse(Aggregation.none)
          fact.aggregation = aggr
          Redirect(routes.Facts.view(factName))
        }.getOrElse(NotFound)
      })
  }

  def get(factName: String, at: Point) = Action {
    val r = for {
      fact ← Fact.get(factName)
      value ← fact.cube.get(at)
    } yield Ok(value)
    r.getOrElse(NotFound)
  }
  def save(factName: String, at: Point) = Action { request ⇒
    request.body.asText.filterNot(_.isEmpty).map { value ⇒
      Fact.get(factName).map { fact ⇒
        fact.cube match {
          case cube: EditableCube[_] ⇒
            try {
              cube.set(at, value)
              Ok(value)
            } catch {
              case ValueCannotBeSetException(_) ⇒ cannotSet
            }
          case _ ⇒ cannotSet
        }
      }.getOrElse(NotFound)
    }.getOrElse(NotAcceptable)
  }
  private def cannotSet = MethodNotAllowed.withHeaders("Allow" -> "GET")

  val addForm = Form("name" -> nonEmptyText)
  val aggrForm = Form("aggregation" -> text)
}