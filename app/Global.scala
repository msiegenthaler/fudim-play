import scala.util.Random
import play.api._
import base._
import cube._
import models._
import models.playbinding._

object Global extends GlobalSettings {
  //so they get registered
  val aggregation = Aggregation.all

  override def onStart(app: Application) {
    if (!InitialData.hasData) {
      Fudim.exec {
        InitialData.insertExample
      }
    }
  }
}

object InitialData {
  def hasData = !DomainRepo.get("example").isEmpty

  def insertExample: Tx = for {
    example <- DomainRepo.create("example")

    monat <- example.dimensionRepo.create("Monat")
    _ <- List("Jan", "Feb", "Mar", "Apr", "Mai").map(monat.add).sequence

    projekt <- example.dimensionRepo.create("Projekt")
    _ <- List("BZ", "AB", "GG").map(projekt.add).sequence

    kostenart <- example.dimensionRepo.create("Kostenart")
    ka_ma <- kostenart.add("Mitarbeiter")
    ka_ext <- kostenart.add("Externe")
    ka_mat <- kostenart.add("Material")
    ka_gk <- kostenart.add("Gemeinkosten")

    umsatzFact <- example.factRepo.createDatabaseBacked("Umsatz", FudimDataTypes.integer, Set(monat, projekt), Aggregation.sum)
    umsatz = umsatzFact.editor.getOrElse(throw new IllegalStateException("Umsatz not editable"))
    kostenFact <- example.factRepo.createDatabaseBacked("Kosten", FudimDataTypes.integer, Set(monat, projekt, kostenart), Aggregation.sum)
    kosten = kostenFact.editor.getOrElse(throw new IllegalStateException("Kosten not editable"))

    gewinnFormula = FudimFormulas.subtract("Umsatz" :: "Kosten" :: Nil, monat :: projekt :: Nil)
    gewinnFact <- example.factRepo.createFormulaBased("Gewinn", FudimDataTypes.integer, gewinnFormula, Aggregation.sum)

    rnd = new Random(1)
    _ <- {
      monat.all.map { m =>
        projekt.all.map { p =>
          val at = m + p
          val k = rnd.nextInt(1000) + 8500

          umsatz.set(at, rnd.nextInt(1000) + 9500) >>
            kosten.set(at + ka_ma, (k * 0.7).round) >>
            kosten.set(at + ka_ext, (k * 0.05).round) >>
            kosten.set(at + ka_mat, 0) >>
            kosten.set(at + ka_gk, (k * 0.25).round)
        }.sequence
      }.sequence
    }
  } yield ()
}
