package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(Fact.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(Fact.all, errors)),
      name ⇒ {
        val fact = Fact(name, Set.empty)
        Fact.save(fact)
        Redirect(routes.Facts.view(name))
      })
  }

  def view(name: String) = Action {
    Fact.find(name).map { fact ⇒
      val dims = Dimension.all.filterNot(fact.dimensions.contains)
      Ok(views.html.fact(fact, dims))
    }.getOrElse(NotFound)
  }
  def addDimension(factName: String, dimensionName: String) = Action {
    Fact.find(factName).map { fact ⇒
      val f2 = fact.copy(dimensions = fact.dimensions + Dimension(dimensionName))
      Fact.save(f2)
      Redirect(routes.Facts.view(factName))
    }.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    Fact.find(factName).map { fact ⇒
      val f2 = fact.copy(dimensions = fact.dimensions - Dimension(dimensionName))
      Fact.save(f2)
      Redirect(routes.Facts.view(factName))
    }.getOrElse(NotFound)
  }

  val addForm = Form("name" -> nonEmptyText)
}