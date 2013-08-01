package models

import play.api.libs.json._
import scala.annotation.implicitNotFound

package object cube {
  @implicitNotFound("${C} cannot be serialized to json")
  trait JsonSerializable[-C] {
    def serialize(cube: C): JsValue
  }
  @implicitNotFound("${C} cannot be parsed json")
  trait JsonParsable[+C] {
    def parse(json: JsValue, soFar: Option[Cube[_]]): Option[Cube[_]]
  }

  trait JsonMapper[C] extends JsonSerializable[C] with JsonParsable[C]
}