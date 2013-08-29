package cube

import play.api.libs.json._
import models._
import support.{ JsonMapper, JsonMapperRepository }

/** Decorator for a cube, for use with DecoratedCube.apply(). */
trait CubeDecorator[T] {
  def get(decoratee: Cube[T])(at: Point) = decoratee.get(at)
  def dense(decoratee: Cube[T]) = decoratee.dense
  def sparse(decoratee: Cube[T]) = decoratee.sparse
}
/** Decorator for a cube that also handles changes to the cube. Use with DecoratedCube.apply(). */
trait EditableCubeDecorator[T] extends CubeDecorator[T] {
  def isSettable(decoratee: EditableCube[T])(at: Point) = decoratee.isSettable(at)
  def set(decoratee: EditableCube[T])(at: Point, value: Option[T]) = decoratee.set(at, value)
  def setAll(decoratee: EditableCube[T])(value: Option[T]) = decoratee.setAll(value)
}
object EditableCubeDecorator {
  def from[T](d: CubeDecorator[T]): EditableCubeDecorator[T] = d match {
    case d: EditableCubeDecorator[T] ⇒ d
    case d ⇒ new EditableCubeDecorator[T] with WrappedCubeDecorator[T] {
      override protected[cube] def unwrap = d
      override def get(decoratee: Cube[T])(at: Point) = d.get(decoratee)(at)
      override def dense(decoratee: Cube[T]) = d.dense(decoratee)
      override def sparse(decoratee: Cube[T]) = d.sparse(decoratee)
      override def toString = d.toString
    }
  }
}

private[cube] trait WrappedCubeDecorator[T] extends CubeDecorator[T] {
  protected[cube] def unwrap: CubeDecorator[T]
}

trait CubeDecoratorCube[T] extends DecoratedCube[T] {
  protected[cube] val decorator: CubeDecorator[T]
}

object CubeDecorator {
  def apply[T](cube: Cube[T], decorator: CubeDecorator[T]): CubeDecoratorCube[T] = cube match {
    case cube: EditableCube[T] ⇒ new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
    case cube ⇒ new CubeWithDecorator(cube, decorator)
  }
  def apply[T](cube: EditableCube[T], decorator: CubeDecorator[T]): CubeDecoratorCube[T] with EditableCube[T] = {
    new EditableCubeWithDecorator(cube, EditableCubeDecorator.from(decorator))
  }
  def unapply[T](cube: Cube[T]): Option[(Cube[T], CubeDecorator[T])] = cube match {
    case c: CubeDecoratorCube[T] ⇒ Some(c.underlying, unwrap(c.decorator))
    case _ ⇒ None
  }

  /** Removes the outermost decorator from the cube. */
  def undecorate[T](cube: Cube[T]): Cube[T] = cube match {
    case c: CubeWithDecorator[T] ⇒ c.underlying
    case c: EditableCubeWithDecorator[T] ⇒ c.underlying
    case c ⇒ c
  }
  /** Removes the outermost decorator from the cube. */
  def undecorate[T](cube: EditableCube[T]): EditableCube[T] = cube match {
    case c: EditableCubeWithDecorator[T] ⇒ c.underlying
    case c ⇒ c
  }
  /** Removes all decorators from the cube. */
  def undecorateComplete[T](cube: Cube[T]): Cube[T] = cube match {
    case c: CubeWithDecorator[T] ⇒ undecorateComplete(c.underlying)
    case c: EditableCubeWithDecorator[T] ⇒ undecorateComplete(c.underlying)
    case c ⇒ c
  }
  /** Removes all decorators from the cube. */
  def undecorateComplete[T](cube: EditableCube[T]): EditableCube[T] = cube match {
    case c: EditableCubeWithDecorator[T] ⇒ undecorateComplete(c.underlying)
    case c ⇒ c
  }

  /** Unwraps cube decorators. */
  private def unwrap[T](d: CubeDecorator[T]): CubeDecorator[T] = d match {
    case d: WrappedCubeDecorator[T] ⇒ unwrap(d.unwrap)
    case d ⇒ d
  }

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

  private class CubeWithDecorator[T](val underlying: Cube[T], val decorator: CubeDecorator[T]) extends CubeDecoratorCube[T] {
    override protected type Self = CubeWithDecorator[T]
    override type Underlying = Cube[T]
    private def wrap(c: Cube[T]) = new CubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
    override def toString = s"CubeWithDecorator($underlying, $decorator)"
  }
  private class EditableCubeWithDecorator[T](val underlying: EditableCube[T], val decorator: EditableCubeDecorator[T]) extends CubeDecoratorCube[T] with EditableCube[T] {
    override protected type Self = EditableCubeWithDecorator[T]
    override type Underlying = EditableCube[T]
    private def wrap(c: EditableCube[T]) = new EditableCubeWithDecorator(c, decorator)

    override def get(at: Point) = decorator.get(underlying)(at)
    override def dense = decorator.dense(underlying)
    override def sparse = decorator.sparse(underlying)
    override def slice = underlying.slice
    override def dimensions = underlying.dimensions
    override def raw = wrap(underlying.raw)
    override def slice(to: Point) = wrap(underlying.slice(to))
    override def dice(dimension: Dimension, filter: Coordinate ⇒ Boolean) = wrap(underlying.dice(dimension, filter))
    override def isSettable(at: Point) = decorator.isSettable(underlying)(at)
    override def set(at: Point, value: Option[T]) = decorator.set(underlying)(at, value)
    override def setAll(value: Option[T]) = decorator.setAll(underlying)(value)
    override def toString = s"EditableCubeWithDecorator($underlying, $decorator)"
  }
}

object CubeDecorators {
  /** Decorator that does not change any behaviour. */
  def noop[T]: EditableCubeDecorator[T] = Noop()
  private case class Noop[T]() extends EditableCubeDecorator[T]

  /** Changes all values using f. Aggregate values are not touched. */
  def mapValue[T](f: T ⇒ T) = new CubeDecorator[T] {
    override def get(decoratee: Cube[T])(at: Point) = {
      if (at.definesExactly(decoratee.dimensions)) decoratee.get(at).map(f)
      else decoratee.get(at)
    }
    override def dense(decoratee: Cube[T]) = decoratee.dense.map(v ⇒ (v._1, v._2.map(f)))
    override def sparse(decoratee: Cube[T]) = decoratee.sparse.map(v ⇒ (v._1, f(v._2)))
  }
  /** Changes all values using f. Aggregate values are not touched. */
  def mapValueOption[T](f: Option[T] ⇒ Option[T]) = new CubeDecorator[T] {
    override def get(decoratee: Cube[T])(at: Point) = {
      val v = decoratee.get(at)
      if (at.definesExactly(decoratee.dimensions)) f(v) else v
    }
    override def dense(decoratee: Cube[T]) = decoratee.dense.map(v ⇒ (v._1, f(v._2)))
    override def sparse(decoratee: Cube[T]) = dense(decoratee).flatMap(e ⇒ e._2.map((e._1, _)))
  }
}