package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import cube._
import models._
import views.html.defaultpages.notFound
import support.FactAction
import support.PointDefinition

object Tables extends Controller {
  def show(domainName: String, factName: String, d1Name: String, d2Name: String, fixed: PointDefinition = PointDefinition.empty, sum1: Boolean = false, sum2: Boolean = false) = FactAction(domainName, factName) { fact ⇒
    val r = for {
      d1 ← fact.dimensions.find(_.name == d1Name)
      d2 ← fact.dimensions.find(_.name == d2Name)
    } yield {
      val filterDims = fact.dimensions - d1 - d2
      val filter = DimensionsFilter(filterDims.map { d ⇒
        fixed(fact).coordinate(d).map(c ⇒ DimensionSelection(d, (c, d.render(c)))).getOrElse(DimensionUnrestricted(d))
      }.toList)
      val cube = Cube.editable(fact.data)
      Ok(views.html.table(domainName, fact, fact.rendered, cube.isSettable _, d1, d2, filter, sum1, sum2))
    }
    r.getOrElse(NotFound)
  }
}

sealed trait DimensionRestriction {
  val dimension: Dimension
  def matches(value: String): Boolean
  def label: String
}
case class DimensionSelection(dimension: Dimension, value: (Coordinate, String)) extends DimensionRestriction {
  override def matches(v: String) = v == value
  override def label = value._2
}
case class DimensionUnrestricted(dimension: Dimension) extends DimensionRestriction {
  override def matches(v: String) = true
  override def label = "<all>"
}

case class DimensionsFilter(restrictions: List[DimensionRestriction]) {
  def availableRestrictionsFor(d: Dimension): List[DimensionRestriction] = {
    val v = d.values.toList
    v.length match {
      case 0 ⇒ DimensionUnrestricted(d) :: Nil
      case 1 ⇒ DimensionSelection(d, v.head) :: Nil
      case _ ⇒ DimensionUnrestricted(d) :: v.map(DimensionSelection(d, _))
    }
  }

  def change(r: DimensionRestriction) = {
    copy(restrictions = restrictions.filterNot(_.dimension == r.dimension) :+ r)
  }

  def point: Point = {
    restrictions.foldLeft(Point.empty) { (p, r) ⇒
      r match {
        case DimensionSelection(d, v) ⇒ p + v._1
        case DimensionUnrestricted(_) ⇒ p
      }
    }
  }
}