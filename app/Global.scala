import play.api._
import models._
import scala.util.Random

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

    val umsatz = Fact.create("Umsatz", Set(monat, project))
    val kosten = Fact.create("Kosten", Set(monat, project, kostenart))

    val rnd = new Random(1)
    for (m ← monat.points) {
      for (p ← project.points) {
        val at = m ++ p
        umsatz.set(at, (rnd.nextInt(1000) + 9500).toString)

        val k = rnd.nextInt(1000) + 8500
        kosten.set(at + (kostenart -> ka_ma), (k * 0.7).round.toString)
        kosten.set(at + (kostenart -> ka_ext), (k * 0.05).round.toString)
        kosten.set(at + (kostenart -> ka_mat), "0")
        kosten.set(at + (kostenart -> ka_gk), (k * 0.25).round.toString)
      }
    }
  }
}