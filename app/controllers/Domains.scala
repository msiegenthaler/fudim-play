package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import support.DomainAction
import models.playbinding.{Fudim, DomainRepo}

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
        Fudim.exec {
          DomainRepo.create(name)
        }
        Redirect(routes.Domains.index)
      })
  }

  val addForm = Form("name" -> nonEmptyText)
}
