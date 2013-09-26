package cube

/** Change values for a cube data structure. */
trait CubeEditor[-T] {
  /** Whether the value at this point can be set. */
  def isSettable(at: Point): Boolean
  /** Set the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def set(at: Point, value: Option[T]): Unit

  /** Set the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def set(at: Point, value: T): Unit = set(at, Some(value))
  /** Remove the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
  def remove(at: Point) = set(at, None)

  /** Set data in this cube. Slice/dice does apply (non-matching are not changed). */
  def multiSet(filter: Point, value: Option[T]): Unit
  /** Remove all data in this cube. Slice/dice does apply (non-matching are not deleted). */
  def clear = multiSet(Point.empty, None)

  def map[A](f: A => T): CubeEditor[A] = {
    val parent = this
    new CubeEditor[A] {
      override def set(at: Point, value: Option[A]) = parent.set(at, value.map(f))
      override def isSettable(at: Point) = parent.isSettable(at)
      override def multiSet(filter: Point, value: Option[A]) = parent.multiSet(filter, value.map(f))
    }
  }
}

object CubeEditor {
  def readOnly[T]: CubeEditor[T] = new CubeEditor[T] {
    override def isSettable(at: Point) = false
    /** Set the value at the point. Throws ValueCannotBeSetException if isSettable for this point is false. */
    override def set(at: Point, value: Option[T]) = throw ValueCannotBeSetException(at)
    override def multiSet(filter: Point, value: Option[T]) = throw ValueCannotBeSetException(filter)
  }
}

case class ValueCannotBeSetException(at: Point) extends RuntimeException(s"Cannot set value at $at")
