package models.cube

import play.api.libs.json._
import models.cube._
import models.cube.db.DatabaseCube

/** Creates a cube from a json configuration. */
trait JsonCubeFactory extends Function1[JsValue, Option[Cube[_]]] {
  protected val baseParsers: Seq[Jsonizable[_]]

  override def apply(config: JsValue) = {
    baseParsers.foldLeft[Option[Cube[_]]](None)((cube, json) ⇒ json.parse(config, cube))
  }
}