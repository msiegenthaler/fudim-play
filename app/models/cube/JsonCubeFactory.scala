package models.cube

import play.api.libs.json._
import models.cube._
import models.cube.db.DatabaseCube

/** Creates a cube from a json configuration. */
trait JsonCubeFactory extends Function1[JsValue, Option[JsonCube[_]]] {
  protected val baseParsers: Seq[JsonCubeParser]

  override def apply(config: JsValue) = {
    baseParsers.foldLeft[Option[JsonCube[_]]](None)((cube, parser) ⇒ parser(config, cube))
  }
}