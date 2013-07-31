package models

import play.api.libs.json.JsValue

package object cube {
  /** Creates a cube from a json configuration. */
  type JsonCubeParser = JsValue ⇒ Option[Cube[_]]
}