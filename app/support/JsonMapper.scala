package support

import scalaz._
import scalaz.Scalaz._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

/** Serialize/parse a object from/into a json structure. The id is used to lookup the parser on deserialization. */
trait JsonMapper[O] {
  val id: String
  def parser: JsValue ⇒ Validation[String, O]
  def serializer: PartialFunction[O, Validation[String, JsValue]]
}

/** Contains multiple JsonMappers and used the appropriate to do serialization/deserialization. */
trait JsonMapperRepository[O] {
  /** The mappers, ordered by priority (higher first). */
  protected val mappers: Seq[JsonMapper[O]]
  private lazy val map = mappers.map(m ⇒ (m.id, m)).toMap

  def parse(json: JsValue): Validation[String, O] = json match {
    case o: JsObject ⇒
      for {
        mapper ← o.keys.find(map.keySet.contains).flatMap(map.get).toSuccess(s"No mapper found for $o. Available: ${map.keys}")
        subjson = (o \ mapper.id)
        res ← mapper.parser(subjson).leftMap(mapper.id + " => " + _)
      } yield res
    case other ⇒ s"Invalid structure, expected json object, but was $other".fail
  }
  def serialize(o: O): Validation[String, JsValue] = {
    for {
      mapper ← mappers.view.find(_.serializer.isDefinedAt(o)).toSuccess(s"No mapper can handle $o")
      subjson ← mapper.serializer(o).leftMap(mapper.id + " => " + _)
    } yield Json.obj(mapper.id -> subjson)
  }
}