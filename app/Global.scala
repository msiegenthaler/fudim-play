import scala.util.Random
import play.api._
import models._
import cube._
import models.playbinding.DomainRepo

object Global extends GlobalSettings {
  //so they get registered
  val aggregation = Aggregation.all

  override def onStart(app: Application) {
    if (!InitialData.hasData) InitialData.insert()
  }
}

object InitialData {
  def hasData = !DomainRepo.get("example").isEmpty
  def insert() {
    val example = DomainRepo.create("example")

    val monat = example.dimensionRepo.create("Monat")
    monat.add("Jan")
    monat.add("Feb")
    monat.add("Mar")
    monat.add("Apr")
    monat.add("Mai")

    val project = example.dimensionRepo.create("Projekt")
    project.add("BZ")
    project.add("AB")
    project.add("GG")

    val kostenart = example.dimensionRepo.create("Kostenart")
    val ka_ma = kostenart.add("Mitarbeiter")
    val ka_ext = kostenart.add("Externe")
    val ka_mat = kostenart.add("Material")
    val ka_gk = kostenart.add("Gemeinkosten")

    val umsatz = example.factRepo.createDatabaseBacked("Umsatz", FudimDataTypes.integer, Set(monat, project), Aggregation.sum).
      editor.getOrElse(throw new IllegalStateException("Umsatz not editable"))
    val kosten = example.factRepo.createDatabaseBacked("Kosten", FudimDataTypes.integer, Set(monat, project, kostenart), Aggregation.sum).
      editor.getOrElse(throw new IllegalStateException("Kosten not editable"))

    val gewinnFormula = FudimFormulas.subtract("Umsatz" :: "Kosten" :: Nil, monat :: project :: Nil)
    val gewinn = example.factRepo.createFormulaBased("Gewinn", FudimDataTypes.integer, gewinnFormula, Aggregation.sum)

    val rnd = new Random(1)
    for (m ← monat.all) {
      for (p ← project.all) {
        val at = m + p
        umsatz.set(at, rnd.nextInt(1000) + 9500)

        val k = rnd.nextInt(1000) + 8500
        kosten.set(at + ka_ma, (k * 0.7).round)
        kosten.set(at + ka_ext, (k * 0.05).round)
        kosten.set(at + ka_mat, 0)
        kosten.set(at + ka_gk, (k * 0.25).round)
      }
    }
  }
}
