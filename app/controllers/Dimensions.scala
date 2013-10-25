package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import base._
import support.DomainAction
import support.DimensionAction
import models.playbinding.Fudim

object Dimensions extends Controller {
  def index(domainName: String) = DomainAction(domainName) { req ⇒
    Ok(views.html.dimensions(domainName, req.fudimDomain.dimensions, addForm))
  }

  def add(domainName: String) = DomainAction(domainName) { implicit req ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimensions(domainName, req.fudimDomain.dimensions, errors)),
      name ⇒ {
        Fudim.execTx {
          req.fudimDomain.dimensionRepo.create(name)
        }
        Redirect(routes.Dimensions.index(domainName))
      })
  }

  def get(domainName: String, name: String) = DimensionAction(domainName, name) { req ⇒
    Ok(views.html.dimension(domainName, req.dimension, req.dimension.values, addValueForm))
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
