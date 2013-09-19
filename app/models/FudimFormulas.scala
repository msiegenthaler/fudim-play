package models

import cube.{DimensionRepository, Dimension}
import domain._
import PointFoldFormula.FoldFunction
import support.{ObjectJsonMapper, JsonMapperRepository, JsonMapper}

object FudimFormulas {
  def add(of: Traversable[String], over: Traversable[Dimension]): Formula[Long] = {
    PointFoldFormula(sumFun, FudimDataTypes.integer)(of, FudimDataTypes.integer, over)
  }

  private val sumFun: FoldFunction[Long, Long] = { vs =>
    Some(vs.view.filter(_.isDefined).map(_.get).foldLeft(0L)(_ + _))
  }

  private val funMapperRepo = new JsonMapperRepository[FoldFunction[_, _]] {
    def o(name: String, f: FoldFunction[_, _]): JsonMapper[FoldFunction[_, _]] =
      ObjectJsonMapper(name, sumFun)
    override val mappers = o("sum", sumFun) :: Nil
  }

  def json(dataTypeRepo: DataTypeRepository, dimensionRepo: DimensionRepository): List[JsonFormulaMapper] = {
    PointFoldFormula.json(dataTypeRepo, dimensionRepo, funMapperRepo) ::
      Nil
  }

}
