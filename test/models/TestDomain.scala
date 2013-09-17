package models

import cube._

case class TestFact[T](name: String, data: Cube[T], dataType: DataType[T]) extends FudimFact[T] {
  override protected def updateCube(aggregation: Aggregation[T]) = throw new UnsupportedOperationException
  override def addDimension(moveTo: Coordinate) = throw new UnsupportedOperationException
  override def removeDimension(keepAt: Coordinate) = throw new UnsupportedOperationException
}

case class TestFactRepo(all: List[FudimFact[_]]) extends FudimFactRepo {
  def get[T](name: String) = all.find(_.name == name)
  def createDatabaseBacked[T](name: String, dataType: DataType[T], dimensions: Set[Dimension], aggregator: Option[Aggregator[T]]) = throw new UnsupportedOperationException
  def remove(name: String) = throw new UnsupportedOperationException
}

case class TestDimensionRepo(all: List[FudimDimension]) extends FudimDimensionRepo {
  def get(name: String) = all.find(_.name == name)
  def create(name: String) = throw new UnsupportedOperationException
  def remove(name: String) = throw new UnsupportedOperationException
}

case class TestDomain(nr: Int, name: String, fs: List[FudimFact[_]]) extends FudimDomain {
  override val id = DomainId(nr)
  override val factRepo = TestFactRepo(fs)
  override val dimensionRepo = TestDimensionRepo(fs.flatMap(_.dimensions).map(_.asInstanceOf[FudimDimension]))
}
