package models.playbinding

import anorm.SqlParser._
import models.dbcube._
import models.{FudimDataTypes, dbcube}

object StoreDataTypes {
  object string extends StoreDataType[String] {
    val dataType = FudimDataTypes.string
    val sqlType = "varchar(1024)"
    def fromDb(name: String) = str(name)
    def toDb(value: String) = value
  }
  object integer extends StoreDataType[Long] {
    val dataType = FudimDataTypes.integer
    val sqlType = "bigint"
    def fromDb(name: String) = long(name)
    def toDb(value: Long) = value
  }

  val all = string :: integer :: Nil
}
