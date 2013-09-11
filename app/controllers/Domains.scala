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
    Ok(views.html.domains(DomainRepo.all))
  }

  def get(name: String) = DomainAction(name) { domain ⇒
    Ok(views.html.domain(domain))
  }
}