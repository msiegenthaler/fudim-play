import scala.util.Random
import play.api._
import models._
import cube._
import support.JsonMapper

object Global extends GlobalSettings {
  val aggregation = Aggregation.all //so they get registered
  override def onStart(app: Application) {
    if (!InitialData.hasData) InitialData.insert
  }
}

object InitialData {
  def hasData = !DimensionRepo.all.isEmpty
  def insert {
    val monat = DimensionRepo.create("Monat")
    monat.add("Jan")
    monat.add("Feb")
    monat.add("Mar")
    monat.add("Apr")
    monat.add("Mai")

    val project = DimensionRepo.create("Projekt")
    project.add("BZ")
    project.add("AB")
    project.add("GG")

    val kostenart = DimensionRepo.create("Kostenart")
    val ka_ma = kostenart.add("Mitarbeiter")
    val ka_ext = kostenart.add("Externe")
    val ka_mat = kostenart.add("Material")
    val ka_gk = kostenart.add("Gemeinkosten")

    val umsatz = FactRepo.createDatabaseBacked("Umsatz", DataType.integer, Set(monat, project), Aggregation.sum.aggregator)
    val kosten = FactRepo.createDatabaseBacked("Kosten", DataType.integer, Set(monat, project, kostenart), Aggregation.sum.aggregator)

    val rnd = new Random(1)
    for (m ← monat.all) {
      for (p ← project.all) {
        val at = m + p
        umsatz.data.set(at, rnd.nextInt(1000) + 9500)

        val k = rnd.nextInt(1000) + 8500
        kosten.data.set(at + ka_ma, (k * 0.7).round)
        kosten.data.set(at + ka_ext, (k * 0.05).round)
        kosten.data.set(at + ka_mat, 0)
        kosten.data.set(at + ka_gk, (k * 0.25).round)
      }
    }
  }

  private implicit def c2ec[T](cube: Cube[T]): EditableCube[T] = EditableCube.from(cube)
}