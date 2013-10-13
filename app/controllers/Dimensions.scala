package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import base._
import support.DomainAction
import support.DimensionAction
import models.playbinding.Fudim

object Dimensions extends Controller {
  def index(domainName: String) = DomainAction(domainName) { domain ⇒
    Ok(views.html.dimensions(domainName, domain.dimensions, addForm))
  }

  def add(domainName: String) = DomainAction(domainName).on(domain ⇒ { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimensions(domainName, domain.dimensions, errors)),
      name ⇒ {
        Fudim.execTx {
          domain.dimensionRepo.create(name)
        }
        Redirect(routes.Dimensions.index(domainName))
      })
  })

  def get(domainName: String, name: String) = DimensionAction(domainName, name) { dimension ⇒
    Ok(views.html.dimension(domainName, dimension, dimension.values, addValueForm))
  }

  def addValue(domainName: String, name: String) = DimensionAction(domainName, name).on(dimension ⇒ { implicit request ⇒
    addValueForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimension(domainName, dimension, dimension.values, errors)),
      value ⇒ {
        Fudim.execTx {
          dimension.add(value)
        }
        Redirect(routes.Dimensions.get(domainName, name))
      })
  })

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}
