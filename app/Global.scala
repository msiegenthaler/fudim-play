import play.api._
import models._

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

    val umsatz = Fact("Umsatz", Set(monat, project))
    Fact.save(umsatz)

    val kosten = Fact("Kosten", Set(monat, project, kostenart))
    Fact.save(kosten)
  }
}