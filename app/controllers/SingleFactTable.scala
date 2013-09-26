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

object SingleFactTable extends Controller {
  def show(domainName: String, factName: String, d1Name: String, d2Name: String, fixed: PointDefinition = PointDefinition.empty, sum1: Boolean = false, sum2: Boolean = false) = FactAction(domainName, factName) { fact ⇒
    val r = for {
      d1 ← fact.dimensions.find(_.name == d1Name)
      d2 ← fact.dimensions.find(_.name == d2Name)
    } yield {
      val otherDims = fact.dimensions - d1 - d2
      val point = fixed(fact)
      val editor = fact.editor.getOrElse(CubeEditor.readOnly)

      def linkFun(d: Dimension)(c: Option[Coordinate]) = {
        val p = c.fold(point - d)(v => point - d + v)
        routes.SingleFactTable.show(domainName, fact.name, d1.name, d2.name, p, sum1, sum2).url
      }

      Ok(views.html.singleFactTable(domainName, fact, fact.rendered, editor.isSettable _, d1, d2, otherDims.toList, point, sum1, sum2, linkFun))
    }
    r.getOrElse(NotFound)
  }
}
