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
    val d = Dimension(name)
    val vs = Dimension.values(d)
    Ok(views.html.dimension(d, vs, addValueForm))
  }

  def addValue(name: String) = Action { implicit request ⇒
    val d = Dimension(name)
    addValueForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.dimension(d, Dimension.values(d), errors)),
      value ⇒ {
        Dimension.addValue(d, value)
        Redirect(routes.Dimensions.get(name))
      })
  }

  val addForm = Form("name" -> nonEmptyText)

  val addValueForm = Form("value" -> nonEmptyText)
}