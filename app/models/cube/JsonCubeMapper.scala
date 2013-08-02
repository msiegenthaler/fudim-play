package models.cube

import play.api.libs.json._

/** Serialize a cube. */
trait JsonCubeMapper {
  val id: String
  def parser: JsValue ⇒ Option[Cube[_]]
  def serializer: PartialFunction[Cube[_], JsValue]
}

trait JsonCubeMapperRepository {
  protected val mappers: Traversable[JsonCubeMapper]
  private lazy val map = mappers.map(m ⇒ (m.id, m)).toMap

  def parse(json: JsValue): Option[Cube[_]] = json match {
    case o: JsObject ⇒
      o.keys.find(map.keySet.contains) flatMap map.get flatMap (m => m.parser(o \ m.id))
    case _ ⇒ None
  }
  def serialize(cube: Cube[_]): Option[JsValue] = {
    mappers.find(_.serializer.isDefinedAt(cube)).map { mapper ⇒
      Json.obj(mapper.id -> mapper.serializer(cube))
    }
  }
}