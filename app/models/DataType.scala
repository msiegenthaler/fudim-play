package models

import scala.util.control.Exception._

trait DataType[T] {
  def name: String
  def tpe: Class[T]

  def render(value: T): String
  def parse(value: String): Option[T]

  def aggregations: List[Aggregation[T]]

  override def toString = name
}

object DataType {
  val all = integer :: string :: Nil

  def get(name: String): Option[DataType[_]] = all.find(_.name == name)

  object integer extends DataType[Long] {
    override def name = "integer"
    override def tpe = classOf[Long]
    override def render(value: Long) = value.toString
    override def parse(value: String) = catching(classOf[NumberFormatException]) opt value.toLong
    override val aggregations = Aggregation.none[Long] :: Aggregation.sum :: Aggregation.product :: Aggregation.average :: Nil
  }

  object string extends DataType[String] {
    override def name = "string"
    override def tpe = classOf[String]
    override def render(value: String) = value
    override def parse(value: String) = Some(value)
    override val aggregations = Aggregation.none[String] :: Aggregation.concat :: Nil
  }
}
