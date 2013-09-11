package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import support.DomainAction
import support.DimensionAction

object Dimensions extends Controller {
  def index(domain: String) = DomainAction(domain) { domain ⇒
    Ok(views.html.dimensions(domain.dimensions, addForm))
  }

  def add(domainName: String) = DomainAction(domainName).on(domain ⇒ { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimensions(domain.dimensions, errors)),
      name ⇒ {
        domain.dimensionRepo.create(name)
        Redirect(routes.Dimensions.index(domainName))
      })
  })

  def get(domainName: String, name: String) = DimensionAction(domainName, name) { dimension ⇒
    Ok(views.html.dimension(dimension, dimension.values, addValueForm))
  }

  def addValue(domainName: String, name: String) = DimensionAction(domainName, name).on(dimension ⇒ { implicit request ⇒
    addValueForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimension(dimension, dimension.values, errors)),
      value ⇒ {
        dimension.add(value)
        Redirect(routes.Dimensions.get(domainName, name))
      })
  })

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}