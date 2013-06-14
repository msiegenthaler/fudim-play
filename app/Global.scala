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
    kostenart.add("Mitarbeiter")
    kostenart.add("Externe")
    kostenart.add("Material")
    kostenart.add("Gemeinkosten")

    val umsatz = DataFact("Umsatz", Set(monat, project))
    Fact.save(umsatz)

    val kosten = DataFact("Kosten", Set(monat, project, kostenart))
    Fact.save(kosten)

    val rnd = new Random(1)
    for (m ← monat.values) {
      for (p ← project.values) {
        val at = Point.empty + (monat -> m) + (project -> p)
        umsatz.set(at, (rnd.nextInt(1000) + 9500).toString)

        val k = rnd.nextInt(1000) + 8500
        kosten.set(at + (kostenart -> "Mitarbeiter"), (k * 0.7).round.toString)
        kosten.set(at + (kostenart -> "Externe"), (k * 0.05).round.toString)
        kosten.set(at + (kostenart -> "Gemeinkosten"), (k * 0.25).round.toString)
      }
    }

  }
}