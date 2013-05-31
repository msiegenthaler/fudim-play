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

  val addForm = Form(
    "name" -> nonEmptyText)

}