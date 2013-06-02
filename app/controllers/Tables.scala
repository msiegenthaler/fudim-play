package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Tables extends Controller {
  def show(factName: String, d1Name: String, d2Name: String) = Action {
    val r = for {
      fact ← Fact.find(factName)
      d1 ← fact.dimensions.find(_.name == d1Name)
      d2 ← fact.dimensions.find(_.name == d2Name)
    } yield Ok(views.html.table(fact, d1, d2))
    r.getOrElse(NotFound)
  }
}