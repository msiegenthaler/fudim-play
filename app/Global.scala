import scala.util.control.Exception._
import scala.util.Random
import play.api._
import models._
import models.cube._
import Point._

object Global extends GlobalSettings {
  override def onStart(app: Application) {
    if (!InitialData.hasData) InitialData.insert
  }
}

object InitialData {
  def hasData = !Dimension.all.isEmpty
  def insert {
    val monat = Dimension.create("Monat")
    monat.add("Jan")
    monat.add("Feb")
    monat.add("Mar")
    monat.add("Apr")
    monat.add("Mai")

    val project = Dimension.create("Projekt")
    project.add("BZ")
    project.add("AB")
    project.add("GG")

    val kostenart = Dimension.create("Kostenart")
    val ka_ma = kostenart.add("Mitarbeiter")
    val ka_ext = kostenart.add("Externe")
    val ka_mat = kostenart.add("Material")
    val ka_gk = kostenart.add("Gemeinkosten")

    val sumAggregator = {
      def sumIfNumber(oa: Option[String], b: String): Option[String] = {
        for {
          a ← oa
          na ← catching(classOf[NumberFormatException]).opt(a.toLong)
          nb ← catching(classOf[NumberFormatException]).opt(b.toLong)
        } yield (na + nb).toString
      }
      Aggregators.fold(Some("0"))(sumIfNumber)
    }

    val umsatz = Fact.createDatabaseBacked("Umsatz", Set(monat, project), Some(sumAggregator))
    val kosten = Fact.createDatabaseBacked("Kosten", Set(monat, project, kostenart), Some(sumAggregator))

    val rnd = new Random(1)
    for (m ← monat.all) {
      for (p ← project.all) {
        val at = m + p
        umsatz.cube.set(at, (rnd.nextInt(1000) + 9500).toString)

        val k = rnd.nextInt(1000) + 8500
        kosten.cube.set(at + ka_ma, (k * 0.7).round.toString)
        kosten.cube.set(at + ka_ext, (k * 0.05).round.toString)
        kosten.cube.set(at + ka_mat, "0")
        kosten.cube.set(at + ka_gk, (k * 0.25).round.toString)
      }
    }
  }

  private implicit def c2ec[T](cube: Cube[T]): EditableCube[T] = EditableCube.from(cube)
}