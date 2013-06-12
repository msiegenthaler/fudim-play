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
    val monat = Dimension("Monat")
    Dimension.create(monat.name)
    Dimension.addValue(monat, "Jan")
    Dimension.addValue(monat, "Feb")
    Dimension.addValue(monat, "Mar")
    Dimension.addValue(monat, "Apr")
    Dimension.addValue(monat, "Mai")

    val project = Dimension("Projekt")
    Dimension.create(project.name)
    Dimension.addValue(project, "BZ")
    Dimension.addValue(project, "AB")
    Dimension.addValue(project, "GG")

    val kostenart = Dimension("Kostenart")
    Dimension.create(kostenart.name)
    Dimension.addValue(kostenart, "Mitarbeiter")
    Dimension.addValue(kostenart, "Externe")
    Dimension.addValue(kostenart, "Material")
    Dimension.addValue(kostenart, "Gemeinkosten")

    val umsatz = DataFact("Umsatz", Set(monat, project))
    Fact.save(umsatz)

    val kosten = DataFact("Kosten", Set(monat, project, kostenart))
    Fact.save(kosten)

    val rnd = new Random(1)
    for (m ← Dimension.values(monat)) {
      for (p ← Dimension.values(project)) {
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