package domain

import org.specs2.specification.Scope
import cube._
import cube.TestFixtures._

object TestFixtures {
  trait dataTypes extends Scope {
    val intType = new DataType[Int] {
      override val name = "Int"
      override val tpe = classOf[Int]
    }
    val stringType = new DataType[String] {
      override val name = "String"
      override val tpe = classOf[String]
    }
    object dataTypeRepo extends DataTypeRepository {
      override val all = intType :: stringType :: Nil
    }
  }
}
