package models.cube

import play.api.libs.json._
import models._
import models.json.JsonMapper
import models.json.JsonMapperRepository

/** Decorator for a cube, for use with DecoratedCube.apply(). */
trait CubeDecorator[D] {
  def get(decoratee: Cube[D])(at: Point) = decoratee.get(at)
  def dense(decoratee: Cube[D]) = decoratee.dense
  def sparse(decoratee: Cube[D]) = decoratee.sparse
}
/** Decorator for a cube that also handles changes to the cube. Use with DecoratedCube.apply(). */
trait EditableCubeDecorator[D] extends CubeDecorator[D] {
  def isSettable(decoratee: EditableCube[D])(at: Point) = decoratee.isSettable(at)
  def set(decoratee: EditableCube[D])(at: Point, value: Option[D]) = decoratee.set(at, value)
  def setAll(decoratee: EditableCube[D])(value: Option[D]) = decoratee.setAll(value)
}
object EditableCubeDecorator {
  def from[D](d: CubeDecorator[D]): EditableCubeDecorator[D] = d match {
    case d: EditableCubeDecorator[D] ⇒ d
    case d ⇒ new EditableCubeDecorator[D] with WrappedCubeDecorator[D] {
      override protected[cube] def unwrap = d
      override def get(decoratee: Cube[D])(at: Point) = d.get(decoratee)(at)
      override def dense(decoratee: Cube[D]) = d.dense(decoratee)
      override def sparse(decoratee: Cube[D]) = d.sparse(decoratee)
    }
  }
}

trait WrappedCubeDecorator[D] extends CubeDecorator[D] {
  protected[cube] def unwrap: CubeDecorator[D]
}

trait CubeDecoratorCube[D] extends DecoratedCube[D] {
  val decorator: CubeDecorator[D]
}

object CubeDecorator {
  def apply[D](cube: Cube[D], decorator: CubeDecorator[D]): CubeDecoratorCube[D] = cube match {
    case cube: EditableCube[D] ⇒ new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
    case cube ⇒ new CubeWithDecorator(cube, decorator)
  }
  def apply[D](cube: EditableCube[D], decorator: CubeDecorator[D]): CubeDecoratorCube[D] with EditableCube[D] = {
    new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
  }

  /** Decorator that does not change any behaviour. */
  case class Noop[D]() extends EditableCubeDecorator[D]

  /** Unwraps cube decorators. */
  def unwrap[D](d: CubeDecorator[D]): CubeDecorator[D] = d match {
    case d: WrappedCubeDecorator[D] ⇒ unwrap(d.unwrap)
    case d ⇒ d
  }

  type JsonCubeDecoratorMapper = JsonMapper[CubeDecorator[_]]
  type JsonCubeDecoratorMapperRepository = JsonMapperRepository[CubeDecorator[_]]

  def json(decoratorRepo: JsonCubeDecoratorMapperRepository, cubeRepo: JsonCubeMapperRepository): JsonCubeMapper = new JsonCubeMapper {
    override val id = "cubeDecorator"
    override def parser = json ⇒ {
      for {
        deco ← decoratorRepo.parse(json \ "decorator")
        cube ← cubeRepo.parse(json \ "cube")
      } yield CubeDecorator(cube.asInstanceOf[Cube[Any]], deco.asInstanceOf[CubeDecorator[Any]])
    }
    override def serializer = {
      case cube: CubeDecoratorCube[_] ⇒
        for {
          dec ← decoratorRepo.serialize(unwrap(cube.decorator))
          und ← cubeRepo.serialize(cube.underlying)
        } yield Json.obj("decorator" -> dec, "cube" -> und)
    }
  }

  private class CubeWithDecorator[D](val underlying: Cube[D], val decorator: CubeDecorator[D]) extends CubeDecoratorCube[D] {
    override protected type Self = CubeWithDecorator[D]
    override type Underlying = Cube[D]
    private def wrap(c: Cube[D]) = new CubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
  }
  private class EditableCubeWithDecorator[D](val underlying: EditableCube[D], val decorator: EditableCubeDecorator[D]) extends CubeDecoratorCube[D] with EditableCube[D] {
    override protected type Self = EditableCubeWithDecorator[D]
    override type Underlying = EditableCube[D]
    private def wrap(c: EditableCube[D]) = new EditableCubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
    override def isSettable(at: Point) = decorator.isSettable(underlying)(at)
    override def set(at: Point, value: Option[D]) = decorator.set(underlying)(at, value)
    override def setAll(value: Option[D]) = decorator.setAll(underlying)(value)
  }
}