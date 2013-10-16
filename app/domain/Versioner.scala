package domain

import base._

/** Creates a version per transaction. */
trait Versioner extends TransactionalRessource {
  protected def createVersion(): Version@tx

  def version: Version@tx = execute {
    case s: VersionerState =>
      s.version.map { v =>
        (s, Right(v))
      }.getOrElse {
        val (s2: VersionerState, r) = createVersion().transaction.run(s)
        r match {
          case ok@Right(version) => (s2.withVersion(Some(version)), ok)
          case error => (s2, error)
        }
      }
    case _ => throw new AssertionError("Unsupported TransactionState: Does not implement VersionerState")
  }.tx
}

trait VersionerState extends TransactionState {
  def version: Option[Version]
  def withVersion(version: Option[Version]): VersionerState
}
