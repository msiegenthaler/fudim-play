package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import support.DomainAction
import support.DimensionAction
import models.playbinding.DomainRepo

object Domains extends Controller {
  def index = Action {
    Ok(views.html.domains(DomainRepo.all, addForm))
  }

  def get(name: String) = DomainAction(name) { domain ⇒
    Ok(views.html.domain(domain))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.domains(DomainRepo.all, errors)),
      name ⇒ {
        DomainRepo.create(name)
        Redirect(routes.Domains.index)
      })
  }

  val addForm = Form("name" -> nonEmptyText)
}