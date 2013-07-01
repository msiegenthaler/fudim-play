package models.cube.db

import models._

private trait CubeType {
  val tpeName: String
  val tpeClass: Class[_]
  def apply(table: String, dims: Map[Dimension, String]): DCDBase[_]
  override def toString = tpeName
}
