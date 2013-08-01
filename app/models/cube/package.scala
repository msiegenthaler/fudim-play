package models

import play.api.libs.json._
import scala.annotation.implicitNotFound

package object cube {
  @implicitNotFound("Cube cannot be serialized to json")
  trait Jsonizable[-C <: Cube[_]] {
    def serialize(cube: C): JsValue
    def parse(json: JsValue, soFar: Option[Cube[_]]): Option[Cube[_]]
  }
}