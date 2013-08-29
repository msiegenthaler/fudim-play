package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import cube._
import models._
import models.dbcube.DatabaseCube

object Facts extends Controller {

  def list = Action {
    Ok(views.html.facts(FactRepo.all, addForm))
  }

  def add = Action { implicit request ⇒
    addForm.bindFromRequest.fold(
      errors ⇒ BadRequest(views.html.facts(FactRepo.all, errors)),
      name ⇒ {
        //TODO let the user choose the data-type
        val fact = FactRepo.createDatabaseBacked(name, DataType.string, Set.empty, None)
        Redirect(routes.Facts.view(name))
      })
  }

  def view(name: String) = Action {
    FactRepo.get(name).map { fact ⇒
      val dims = DimensionRepo.all.filterNot(fact.dimensions.contains)
      val aggr = Aggregation.unapply(fact.data).getOrElse(Aggregation.none)
      Ok(views.html.fact(fact, dims, fact.dataType.aggregations, aggrForm.fill(aggr.name)))
    }.getOrElse(NotFound)
  }
  def addDimension(factName: String, dimensionName: String) = Action {
    val r = for {
      fact ← FactRepo.get(factName)
      dimension ← DimensionRepo.get(dimensionName)
      moveTo ← dimension.all.headOption
    } yield {
      fact.addDimension(moveTo)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }
  def removeDimension(factName: String, dimensionName: String) = Action {
    val r = for {
      fact ← FactRepo.get(factName)
      dimension ← DimensionRepo.get(dimensionName)
      keepAt ← dimension.all.headOption
    } yield {
      fact.removeDimension(keepAt)
      Redirect(routes.Facts.view(factName))
    }
    r.getOrElse(NotFound)
  }

  def setAggregation(factName: String) = Action { implicit request ⇒
    aggrForm.bindFromRequest.fold(
      errors ⇒
        NotImplemented,
      aggrName ⇒ {
        FactRepo.get(factName).map { fact ⇒
          val aggr = fact.dataType.aggregations.find(_.name == aggrName).getOrElse(Aggregation.none)
          fact.aggregation = aggr
          Redirect(routes.Facts.view(factName))
        }.getOrElse(NotFound)
      })
  }

  def get(factName: String, at: Point) = Action {
    val r = for {
      fact ← FactRepo.get(factName)
      value ← fact.rendered.get(at)
    } yield Ok(value)
    r.getOrElse(NotFound)
  }
  def save(factName: String, at: Point) = Action { request ⇒
    request.body.asText.filterNot(_.isEmpty).map { value ⇒
      FactRepo.get(factName).map { fact ⇒
        fact.data match {
          case cube: EditableCube[_] ⇒
            val tpe = fact.dataType
            tpe.parse(value).map { v ⇒
              try {
                cube.set(at, v)
                Ok(tpe.render(v))
              } catch {
                case ValueCannotBeSetException(_) ⇒ cannotSet
              }
            }.getOrElse(NotAcceptable)
          case _ ⇒ cannotSet
        }
      }.getOrElse(NotFound)
    }.getOrElse(NotAcceptable)
  }
  private def cannotSet = MethodNotAllowed.withHeaders("Allow" -> "GET")

  val addForm = Form("name" -> nonEmptyText)
  val aggrForm = Form("aggregation" -> text)
}