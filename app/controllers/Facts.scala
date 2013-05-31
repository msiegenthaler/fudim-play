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
        val fact = Fact(name, Nil)
        Fact.save(fact)
        Redirect(routes.Facts.view(name))
      })
  }

  def view(name: String) = Action {
    Fact.find(name).map(fact => Ok(views.html.fact(fact))).getOrElse(NotFound)
  }

  val addForm = Form("name" -> nonEmptyText)
}