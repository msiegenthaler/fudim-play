package models.cube

import play.api.libs.json._
import models.cube._
import models.cube.db.DatabaseCube

/** Cube that supports serialization of its state to json. */
trait JsonCube[T] extends Cube[T] {
  def asJson: JsValue
}

/** Parser of the basic cube. */
trait JsonCubeParser {
  def parseJson(json: JsValue): Option[JsonCube[_]]
  val jsonType: String
}

/** Creates a cube from a json configuration. */
trait JsonCubeFactory {
  protected def baseParsers: Traversable[JsonCubeParser]
  private lazy val baseMap = baseParsers.map(p ⇒ (p.jsonType, p)).toMap

  def apply(config: JsValue): Option[JsonCube[_]] = {
    val baseConfig = config \ "base"
    for {
      tpe ← (baseConfig \ "type").asOpt[String]
      baseFactory ← baseMap.get(tpe)
      base ← baseFactory.parseJson(baseConfig)
      //TODO aggregator
    } yield base
  }
}