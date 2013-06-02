package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Tables extends Controller {
  def show(factName: String, d1Name: String, d2Name: String) = Action {
    def selectedOf(d: Dimension) = {
      //TODO somewhen also get it from the query string
      Dimension.values(d).headOption.getOrElse("")
    }
    val r = for {
      fact ← Fact.find(factName)
      d1 ← fact.dimensions.find(_.name == d1Name)
      d1Values = Dimension.values(d1)
      d2 ← fact.dimensions.find(_.name == d2Name)
      d2Values = Dimension.values(d2)
      rest = fact.dimensions - d1 - d2
      point = rest.foldLeft(Point.empty)((p, d) ⇒ p + (d -> selectedOf(d)))
    } yield Ok(views.html.table(fact, d1, d1Values, d2, d2Values, point))
    r.getOrElse(NotFound)
  }
}