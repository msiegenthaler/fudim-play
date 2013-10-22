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
      Fudim.execTx {
        InitialData.insertExample
      }
    }
  }
}

object InitialData {
  def hasData = !DomainRepo.get("example").isEmpty

  def insertExample: Unit @tx = {
    Logger.trace("Generating example data..")
    val example = DomainRepo.create("example")

    val monat = example.dimensionRepo.create("Monat")
    List("Jan", "Feb", "Mar", "Apr", "Mai").
      foreachTx(monat.add)

    val projekt = example.dimensionRepo.create("Projekt")
    List("BZ", "AB", "GG").foreachTx(projekt.add)

    val kostenart = example.dimensionRepo.create("Kostenart")
    val ka_ma = kostenart.add("Mitarbeiter")
    val ka_ext = kostenart.add("Externe")
    val ka_mat = kostenart.add("Material")
    val ka_gk = kostenart.add("Gemeinkosten")

    val umsatzFact = example.factRepo.createDatabaseBacked("Umsatz", FudimDataTypes.integer, Set(monat, projekt), Aggregation.sum)
    val umsatz = umsatzFact.editor.get
    val kostenFact = example.factRepo.createDatabaseBacked("Kosten", FudimDataTypes.integer, Set(monat, projekt, kostenart), Aggregation.sum)
    val kosten = kostenFact.editor.get

    val gewinnFormula = FudimFormulas.subtract("Umsatz" :: "Kosten" :: Nil, monat :: projekt :: Nil)
    val gewinnFact = example.factRepo.createFormulaBased("Gewinn", FudimDataTypes.integer, gewinnFormula, Aggregation.sum)

    val rnd = new Random(1)
    monat.all.foreachTx { m ⇒
      projekt.all.foreachTx { p ⇒
        val at = m + p
        val k = rnd.nextInt(1000) + 8500

        umsatz.set(at, rnd.nextInt(1000) + 9500)
        kosten.set(at + ka_ma, (k * 0.7).round)
        kosten.set(at + ka_ext, (k * 0.05).round)
        kosten.set(at + ka_mat, 0)
        kosten.set(at + ka_gk, (k * 0.25).round)
      }
    }
    Logger.trace("Generated example data.")
  }
}
