package domain

trait DataType[T] {
  def name: String
  def tpe: Class[T]
  type Type = T

  override def toString = name
}

trait DataTypeRepository {
  def all: TraversableOnce[DataType[_]]
  def get(name: String): Option[DataType[_]]
}
