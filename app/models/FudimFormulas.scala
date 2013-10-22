package models

import cube.{ DimensionRepository, Dimension }
import domain._
import PointFoldFormula.FoldFunction
import support.{ ObjectJsonMapper, JsonMapperRepository, JsonMapper }

object FudimFormulas {
  /** Adds the value at the point on all referenced facts. */
  def add(of: Traversable[String], over: Traversable[Dimension]): Formula[Long] = {
    PointFoldFormula(sumFun, FudimDataTypes.integer)(of.toSeq, FudimDataTypes.integer, over)
  }
  /** Subtracts from the value of the first fact the values of all other facts at the point. */
  def subtract(of: Seq[String], over: Traversable[Dimension]): Formula[Long] = {
    PointFoldFormula(differenceFun, FudimDataTypes.integer)(of, FudimDataTypes.integer, over)
  }

  private val sumFun: FoldFunction[Long, Long] = { vs ⇒
    Some(vs.view.filter(_.isDefined).map(_.get).foldLeft(0L)(_ + _))
  }
  private val differenceFun: FoldFunction[Long, Long] = { vs ⇒
    vs.headOption.map(_.get).map(_ - sumFun(vs.tail).getOrElse(0L))
  }

  private val funMapperRepo = new JsonMapperRepository[FoldFunction[_, _]] {
    def o(name: String, f: FoldFunction[_, _]): JsonMapper[FoldFunction[_, _]] =
      ObjectJsonMapper(name, f)
    override val mappers = o("sum", sumFun) :: o("difference", differenceFun) :: Nil
  }

  def json(dataTypeRepo: DataTypeRepository, dimensionRepo: DimensionRepository): List[JsonFormulaMapper] = {
    PointFoldFormula.json(dataTypeRepo, dimensionRepo, funMapperRepo) ::
      Nil
  }

}
