package models

import play.api.libs.json._

package object cube {
  /** Creates or extends a cube from json. */
  type JsonCubeParser = (JsValue, Option[JsonCube[_]]) ⇒ Option[JsonCube[_]]
}