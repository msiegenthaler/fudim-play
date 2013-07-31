package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import models.cube._

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(Fact.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(Fact.all, errors)),
      name ⇒ {
        val fact = Fact.create(name, Set.empty)
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
    val r = for {
      dimension ← Dimension.get(dimensionName)
      fact ← Fact.get(factName)
    } yield {
      //TODO let user chose coordinate to assign
      fact.addDimension(dimension.all.head)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    val r = for {
      dimension ← Dimension.get(dimensionName)
      fact ← Fact.get(factName)
    } yield {
      //TODO let user chose coordinate to keep
      fact.removeDimension(dimension.all.head)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }

  def get(factName: String, at: Point) = Action {
    val r = for {
      fact ← Fact.get(factName)
      value ← fact.cube.get(at)
    } yield Ok(value)
    r.getOrElse(NotFound)
  }
  def save(factName: String, at: Point) = Action { request ⇒
    request.body.asText.filterNot(_.isEmpty).map { value ⇒
      Fact.get(factName).map { fact ⇒
        fact.cube match {
          case cube: EditableCube[_] ⇒
            try {
              cube.set(at, value)
              Ok(value)
            } catch {
              case ValueCannotBeSetException(_) ⇒ cannotSet
            }
          case _ ⇒ cannotSet
        }
      }.getOrElse(NotFound)
    }.getOrElse(NotAcceptable)
  }
  private def cannotSet = MethodNotAllowed.withHeaders("Allow" -> "GET")

  val addForm = Form("name" -> nonEmptyText)
}