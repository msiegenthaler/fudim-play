package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(Fact.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(Fact.all, errors)),
      name ⇒ {
        val fact = DataFact(name, Set.empty)
        Fact.save(fact)
        Redirect(routes.Facts.view(name))
      })
  }

  def view(name: String) = Action {
    Fact.get(name).map { fact ⇒
      val dims = Dimension.all.filterNot(fact.dimensions.contains)
      Ok(views.html.fact(fact, dims))
    }.getOrElse(NotFound)
  }
  def addDimension(factName: String, dimensionName: String) = Action {
    val dimension = Dimension.get(dimensionName).getOrElse(throw new IllegalArgumentException(s"Dimension dimensionName does not exist"))
    Fact.get(factName).map {
      _ match {
        case fact: DataFact ⇒
          val f2 = fact.copy(dimensions = fact.dimensions + dimension)
          Fact.save(f2)
          Redirect(routes.Facts.view(factName))
        case _ ⇒ throw new IllegalArgumentException
      }
    }.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    val dimension = Dimension.get(dimensionName).getOrElse(throw new IllegalArgumentException(s"Dimension dimensionName does not exist"))
    Fact.get(factName).map {
      _ match {
        case fact: DataFact ⇒
          val f2 = fact.copy(dimensions = fact.dimensions - dimension)
          Fact.save(f2)
          Redirect(routes.Facts.view(factName))
        case _ ⇒ throw new IllegalArgumentException
      }
    }.getOrElse(NotFound)
  }

  def get(factName: String, at: Point) = Action {
    val r = for {
      fact ← Fact.get(factName)
      value ← fact.get(at)
    } yield Ok(value)
    r.getOrElse(NotFound)
  }
  def save(factName: String, at: Point) = Action { request ⇒
    val r = for {
      fact ← Fact.get(factName)
      value = request.body.asText.filterNot(_.isEmpty)
      _ = fact.set(at, value)
    } yield Ok(value.getOrElse(""))
    r.getOrElse(NotFound)
  }

  val addForm = Form("name" -> nonEmptyText)
}