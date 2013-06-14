package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Dimensions extends Controller {

  def index = Action {
    Ok(views.html.dimensions(Dimension.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimensions(Dimension.all, errors)),
      name ⇒ {
        Dimension.create(name)
        Redirect(routes.Dimensions.index)
      })
  }

  def get(name: String) = Action {
    val r = for {
      d ← Dimension.get(name)
      vs = d.values
    } yield Ok(views.html.dimension(d, vs, addValueForm))
    r.getOrElse(NotFound)
  }

  def addValue(name: String) = Action { implicit request ⇒
    (for {
      d ← Dimension.get(name)
    } yield {
      addValueForm.bindFromRequest.fold(
        errors ⇒ BadRequest(views.html.dimension(d, d.values, errors)),
        value ⇒ {
          d.add(value)
          Redirect(routes.Dimensions.get(name))
        })
    }).getOrElse(NotFound)
  }

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}