package domain

import play.api.libs.json._
import support.JsonMapperRepository
import cube._

/** Combines the values of multiple cubes at the same point with a function. */
object PointFoldFormula {
  type FoldFunction[-A, +B] = Traversable[Option[A]] ⇒ Option[B]

  def apply[A, B](f: FoldFunction[A, B], toType: DataType[B])(ofNames: Seq[String], ofType: DataType[A], over: Traversable[Dimension]): Formula[B] = {
    val refs = ofNames.map(CubeRef(_, ofType))
    PointFoldFormulaImpl(f, toType, refs, ofType, over.toSet)
  }

  private case class PointFoldFormulaImpl[A, B](f: FoldFunction[A, B], toType: DataType[B],
    refs: Seq[CubeRef[A]], ofType: DataType[A], dimensions: Set[Dimension]) extends Formula[B] {
    override val references = refs.asInstanceOf[Seq[CubeRef[_]]]
    override def bind(cubes: Cubes) = {
      val cs = refs.map(ref ⇒ (ref, cubes.get(ref)))
      val missing = cs.find(_._2.isEmpty)
      if (!missing.isEmpty) throw new IllegalArgumentException(s"Unresolved dependencies to: ${missing.mkString(", ")}")
      val deps = cs.map(_._2.get)
      new BoundFormula[B] {
        override def get(point: Point) = {
          if (point.definesExactly(dimensions)) {
            val values = deps.map(_.get(point))
            f(values)
          } else None
        }
      }
    }
  }

  def json(dataTypeRepo: DataTypeRepository, dimRepo: DimensionRepository, functionRepo: JsonMapperRepository[FoldFunction[_, _]]): JsonFormulaMapper = new JsonFormulaMapper {
    import scalaz._
    import Scalaz._

    private def parseDataType(json: JsValue) = for {
      name ← json.asOpt[String].toSuccess("Missing dataType")
      dt ← dataTypeRepo.get(name).toSuccess(s"DataType $name does not exist")
    } yield dt
    private def serializeDimensions(dimensions: Traversable[Dimension]) = JsArray(dimensions.map(d ⇒ JsString(d.name)).toSeq)
    private def parseDimensions(names: List[String], soFar: List[Dimension] = Nil): Validation[String, List[Dimension]] = names match {
      case name :: tail ⇒ dimRepo.get(name) match {
        case Some(dimension) ⇒ parseDimensions(tail, dimension :: soFar)
        case None ⇒ s"Dimension $name does not exist".failure
      }
      case Nil ⇒ soFar.success
    }
    private def serializeRefs(refs: Traversable[CubeRef[_]]) = {
      val rs = refs.map(ref ⇒ Json.obj("name" -> ref.name, "dataType" -> ref.dataType.name))
      JsArray(rs.toSeq)
    }
    private def parseRefs(values: List[JsValue], soFar: List[CubeRef[_]] = Nil): Validation[String, List[CubeRef[_]]] = values match {
      case json :: tail ⇒
        val r = for {
          name ← (json \ "name").asOpt[String].toSuccess("Missing name")
          dataType ← parseDataType(json \ "dataType")
        } yield CubeRef(name, dataType)
        r match {
          case Success(ref) ⇒ parseRefs(tail, ref :: soFar)
          case Failure(e) ⇒ s"ref '$json': $e".fail
        }
      case Nil ⇒ soFar.reverse.success
    }

    val id = "pointFoldFormula"
    def parser = json ⇒
      for {
        dataType ← parseDataType(json \ "dataType")
        fun ← functionRepo.parse(json \ "foldFunction")
        dimNames ← (json \ "dimensions").asOpt[List[String]].toSuccess("Missing dimensions")
        dims ← parseDimensions(dimNames)
        of = json \ "of"
        refJsons ← (of \ "refs").asOpt[List[JsValue]].toSuccess("Missing refs")
        refs ← parseRefs(refJsons)
        ofDataType ← parseDataType(of \ "dataType")
        _ ← refs.find(_.dataType != ofDataType).map(t ⇒ "Wrong ref dataType ${t.name}, must be ${ofDateType.name}").toFailure(())
      } yield {
        def mk[A, B] = {
          PointFoldFormulaImpl(
            fun.asInstanceOf[FoldFunction[A, B]],
            dataType.asInstanceOf[DataType[B]],
            refs.asInstanceOf[Seq[CubeRef[A]]],
            ofDataType.asInstanceOf[DataType[A]],
            dims.toSet)
        }
        mk
      }
    def serializer = {
      case PointFoldFormulaImpl(f, toType, refs, ofType, dimensions) ⇒
        for {
          fun ← functionRepo.serialize(f)
          dims = serializeDimensions(dimensions)
          of = Json.obj(
            "dataType" -> ofType.name,
            "refs" -> serializeRefs(refs))
        } yield Json.obj(
          "dataType" -> toType.name,
          "dimensions" -> dims,
          "foldFunction" -> fun,
          "of" -> of)
    }
  }
}
