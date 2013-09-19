package models

import scala.util.control.Exception._
import domain.{DataType, DataTypeRepository}

trait FudimDataType[T] extends DataType[T] {
  def render(value: T): String
  def parse(value: String): Option[T]

  def aggregations: List[Aggregation[T]]
}

object FudimDataTypes extends DataTypeRepository {
  override val all: List[FudimDataType[_]] = integer :: string :: Nil
  override def get(name: String): Option[FudimDataType[_]] = all.find(_.name == name)

  object integer extends FudimDataType[Long] {
    override def name = "integer"
    override def tpe = classOf[Long]
    override def render(value: Long) = value.toString
    override def parse(value: String) = catching(classOf[NumberFormatException]) opt value.toLong
    override val aggregations = Aggregation.none[Long] :: Aggregation.sum :: Aggregation.product :: Aggregation.average :: Nil
  }

  object string extends FudimDataType[String] {
    override def name = "string"
    override def tpe = classOf[String]
    override def render(value: String) = value
    override def parse(value: String) = Some(value)
    override val aggregations = Aggregation.none[String] :: Aggregation.concat :: Nil
  }
}
