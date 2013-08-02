package models.json

import play.api.libs.json._

/** Serialize/parse a object from/into a json structure. The id is used to lookup the parser on deserialization. */
trait JsonMapper[O] {
  val id: String
  def parser: JsValue ⇒ Option[O]
  def serializer: PartialFunction[O, Option[JsValue]]
}

/** Contains multiple JsonMappers and used the appropriate to do serialization/deserialization. */
trait JsonMapperRepository[O] {
  /** The mappers, ordered by priority (higher first). */
  protected val mappers: Seq[JsonMapper[O]]
  private lazy val map = mappers.map(m ⇒ (m.id, m)).toMap

  def parse(json: JsValue): Option[O] = json match {
    case o: JsObject ⇒
      o.keys.find(map.keySet.contains) flatMap map.get flatMap (m ⇒ m.parser(o \ m.id))
    case _ ⇒ None
  }
  def serialize(o: O): Option[JsValue] = {
    mappers.find(_.serializer.isDefinedAt(o)).flatMap { mapper ⇒
      mapper.serializer(o).map(s ⇒ Json.obj(mapper.id -> s))
    }
  }
}