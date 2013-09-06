package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import support.DomainAction

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

  def get(domainName: String, name: String) = DomainAction(domainName) { domain ⇒
    val r = for {
      d ← domain.dimension(name)
      vs = d.values
    } yield Ok(views.html.dimension(d, vs, addValueForm))
    r.getOrElse(NotFound)
  }

  def addValue(domainName: String, name: String) = DomainAction(domainName).on(domain ⇒ { implicit request ⇒
    (for {
      d ← domain.dimensionRepo.get(name)
    } yield {
      addValueForm.bindFromRequest.fold(
        errors ⇒ BadRequest(views.html.dimension(d, d.values, errors)),
        value ⇒ {
          d.add(value)
          Redirect(routes.Dimensions.get(domainName, name))
        })
    }).getOrElse(NotFound)
  })

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}