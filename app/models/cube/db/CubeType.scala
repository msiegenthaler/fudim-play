package models.cube.db

import models._

private trait CubeType {
  val tpeName: String
  val tpeClass: Class[_]
  def apply(id: Long, table: String, dims: Map[Dimension, String]): DatabaseCube[_]
  override def toString = tpeName
}
