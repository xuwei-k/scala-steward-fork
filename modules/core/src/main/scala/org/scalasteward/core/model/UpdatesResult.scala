package org.scalasteward.core.model

sealed abstract class UpdatesResult extends Product with Serializable {
  final def isFail: Boolean = this match {
    case _: UpdatesResult.Failure.type =>
      true
    case _ =>
      false
  }
  final def isSkip: Boolean = this match {
    case _: UpdatesResult.Skip.type =>
      true
    case _ =>
      false
  }
  final def isDoUpdate: Boolean = this match {
    case _: UpdatesResult.DoUpdate.type =>
      true
    case _ =>
      false
  }
}
object UpdatesResult {
  private case object Skip extends UpdatesResult
  private case object DoUpdate extends UpdatesResult
  private case object Failure extends UpdatesResult

  def skip: UpdatesResult = Skip
  def doUpdate: UpdatesResult = DoUpdate
  def failure: UpdatesResult = Failure
}
