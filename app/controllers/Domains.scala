package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.playbinding.{ Fudim, DomainRepo }
import support.{ DomainAction, HttpCache }

object Domains extends Controller {
  def index = Action {
    Ok(views.html.domains(DomainRepo.all, addForm))
  }

  def get(name: String) = DomainAction(name) { req ⇒
    HttpCache.cached(req, req.fudimDomain.version) {
      Ok(views.html.domain(req.fudimDomain))
    }
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.domains(DomainRepo.all, errors)),
      name ⇒ {
        Fudim.execTx {
          DomainRepo.create(name)
        }
        Redirect(routes.Domains.index)
      })
  }

  val addForm = Form("name" -> nonEmptyText)
}
