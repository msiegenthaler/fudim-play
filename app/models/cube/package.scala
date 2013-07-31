package models

import play.api.libs.json._

package object cube {
  /** Cube that supports serialization of its state to json. */
  trait Jsonizable { def asJson: JsValue }

  type JsonCube[T] = Cube[T] with Jsonizable

  /** Creates or extends a cube from json. */
  type JsonCubeParser = (JsValue, Option[JsonCube[_]]) ⇒ Option[JsonCube[_]]
}