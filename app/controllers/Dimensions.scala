package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import base._
import support.{ DomainAction, DimensionAction, HttpCache }
import models.playbinding.Fudim

object Dimensions extends Controller {
  def index(domainName: String) = DomainAction(domainName) { req ⇒
    HttpCache.cached(req, req.fudimDomain.version) {
      Ok(views.html.dimensions(domainName, req.dimensions.all, addForm))
    }
  }

  def add(domainName: String) = DomainAction(domainName) { implicit req ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimensions(domainName, req.dimensions.all, errors)),
      name ⇒ {
        Fudim.execTx {
          req.dimensions.create(name)
        }
        Redirect(routes.Dimensions.index(domainName))
      })
  }

  def get(domainName: String, name: String) = DimensionAction(domainName, name) { req ⇒
    HttpCache.cached(req, req.dimension.version) {
      Ok(views.html.dimension(domainName, req.dimension, req.dimension.values, addValueForm))
    }
  }

  def addValue(domainName: String, name: String) = DimensionAction(domainName, name) { implicit req ⇒
    addValueForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimension(domainName, req.dimension, req.dimension.values, errors)),
      value ⇒ {
        Fudim.execTx {
          req.dimension.add(value)
        }
        Redirect(routes.Dimensions.get(domainName, name))
      })
  }

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}
