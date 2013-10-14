package models

import base._

/** Creates a version per transaction. */
trait Versioner[+Version] extends TransactionalRessource {
  protected def createVersion(): Version@tx

  def version: Version@tx = execute {
    case s: VersionerState[Version] =>
      s.version.map { v =>
        (s, Right(v))
      }.getOrElse {
        val (s2: VersionerState[Version], r) = createVersion().transaction.run(s)
        r match {
          case ok@Right(version) => (s2.withVersion(Some(version)), ok)
          case error => (s2, error)
        }
      }
    case _ => throw new AssertionError("Unsupported TransactionState: Does not implement VersionerState")
  }.tx
}

trait VersionerState[Version] extends TransactionState {
  def version: Option[Version]
  def withVersion(version: Option[Version]): VersionerState[Version]
}
