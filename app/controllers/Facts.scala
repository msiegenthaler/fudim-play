package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import models.cube._
import models.cube.db.DatabaseCube

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(Fact.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(Fact.all, errors)),
      name ⇒ {
        val fact = Fact.createDatabaseBacked(name, Set.empty)
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
    Dimension.get(dimensionName).map { dimension ⇒
      //TODO let user chose coordinate to keep
      modifyDbCube(factName, _.copyAndAddDimension(dimension.all.head))
    }.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    Dimension.get(dimensionName).map { dimension ⇒
      //TODO let user chose coordinate to keep
      modifyDbCube(factName, _.copyAndRemoveDimension(dimension.all.head))
    }.getOrElse(NotFound)
  }
  private def modifyDbCube(factName: String, f: DatabaseCube[String] ⇒ DatabaseCube[String]) = Fact.get(factName) match {
    case Some(fact) ⇒
      fact.cube match {
        case cube: DatabaseCube[String] ⇒
          val newCube = f(cube)
          Fact.assignCube(factName, newCube)
          DatabaseCube.delete(cube)
          Redirect(routes.Facts.view(factName))
        case _ ⇒ MethodNotAllowed
      }
    case None ⇒ NotFound
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