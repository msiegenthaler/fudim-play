package models.cube

import play.api.libs.json._
import models.cube._
import models.cube.db.DatabaseCube

/** Cube that supports serialization of its state to json. */
trait JsonCube[T] extends Cube[T] {
  def asJson: JsValue
}

/** Creates a cube from a json configuration. */
trait JsonCubeFactory extends JsonCubeParser {
  protected val baseFactories: Map[String, JsonCubeParser]

  override def apply(config: JsValue) = {
    val baseConfig = config \ "base"
    for {
      tpe ← (baseConfig \ "type").asOpt[String]
      baseFactory ← baseFactories.get(tpe)
      base ← baseFactory(baseConfig)
      //TODO aggregator
    } yield base
  }
}