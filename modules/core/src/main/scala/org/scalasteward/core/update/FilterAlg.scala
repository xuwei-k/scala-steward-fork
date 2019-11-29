/*
 * Copyright 2018-2019 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.update

import cats.implicits._
import cats.{Monad, TraverseFilter}
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.data.Update
import org.scalasteward.core.repoconfig.RepoConfig
import org.scalasteward.core.update.FilterAlg._
import org.scalasteward.core.util

final class FilterAlg[F[_]](
    implicit
    logger: Logger[F],
    F: Monad[F]
) {
  def globalFilterMany[G[_]: TraverseFilter](updates: G[Update.Single]): F[G[Update.Single]] =
    updates.traverseFilter(update => logIfRejected(globalFilter(update)))

  def localFilterMany[G[_]: TraverseFilter](
      config: RepoConfig,
      updates: G[Update.Single]
  ): F[G[Update.Single]] =
    updates.traverseFilter(update => logIfRejected(localFilter(update, config)))

  private def logIfRejected(result: FilterResult): F[Option[Update.Single]] =
    result match {
      case Right(update) => F.pure(update.some)
      case Left(reason) =>
        logger.info(s"Ignore ${reason.update.show} (reason: ${reason.show})") *> F.pure(None)
    }
}

object FilterAlg {
  type FilterResult = Either[RejectionReason, Update.Single]

  sealed trait RejectionReason {
    def update: Update.Single
    def show: String = this match {
      case IgnoredGlobally(_)             => "ignored globally"
      case IgnoredByConfig(_)             => "ignored by config"
      case NotAllowedByConfig(_)          => "not allowed by config"
      case BadVersions(_)                 => "bad versions"
      case NonSnapshotToSnapshotUpdate(_) => "non-snapshot to snapshot"
    }
  }

  final case class IgnoredGlobally(update: Update.Single) extends RejectionReason
  final case class IgnoredByConfig(update: Update.Single) extends RejectionReason
  final case class NotAllowedByConfig(update: Update.Single) extends RejectionReason
  final case class BadVersions(update: Update.Single) extends RejectionReason
  final case class NonSnapshotToSnapshotUpdate(update: Update.Single) extends RejectionReason

  def globalFilter(update: Update.Single): FilterResult =
    removeBadVersions(update)
      .flatMap(isIgnoredGlobally)
      .flatMap(ignoreNonSnapshotToSnapshotUpdate)

  def localFilter(update: Update.Single, repoConfig: RepoConfig): FilterResult =
    globalFilter(update).flatMap(repoConfig.updates.keep)

  def isIgnoredGlobally(update: Update.Single): FilterResult = {
    val keep = ((update.groupId, update.artifactId) match {
      case ("org.scala-lang", "scala-compiler") => false
      case ("org.scala-lang", "scala-library")  => false
      case ("org.scala-lang", "scala-reflect")  => false
      case ("org.typelevel", "scala-library")   => false
      case _ =>
        (update.groupId, update.artifactId, update.currentVersion, update.newerVersions.head) match {
          case (_, _, _, _) if update.currentVersion.endsWith(")") => false
          case (_, _, current, newer) if (!current.toLowerCase.contains("alpha")) && newer.toLowerCase.contains("alpha") => false
          case (_, _, current, newer) if (!current.toLowerCase.contains("beta")) && newer.toLowerCase.contains("beta") => false

          case ("mysql", "mysql-connector-java", _, v) if v.startsWith("8.") => false
          case ("org.postgresql", "postgresql", _, v) if v.startsWith("42.") => false
          case ("org.postgresql", "postgresql", _, "9.4.1212")               => false
          case ("org.scala-sbt", "sbt-launch", _, _)                         => false

          case ("com.jsuereth", "sbt-pgp", current, next)
              if current.startsWith("1.") && !next.startsWith("1") =>
            false

          case ("org.scalaz.stream", "scalaz-stream", _, "0.8.6") => false

          case ("org.scalaz", _, _, ScalazVersions()) => false

          case ("javax.servlet", "javax.servlet-api", _, _) => false

          // https://github.com/scala/scala-parser-combinators/issues/197
          // https://github.com/sbt/sbt/issues/4609
          case ("org.scala-lang.modules", "scala-parser-combinators", _, "1.1.2") => false

          // argonaut
          case ("com.google.caliper", "caliper", _, _) => false

          case ("com.geirsson", a, _, _) if a.startsWith("scalafmt-core") => false

          // transitive dependencies of e.g. com.lucidchart:sbt-scalafmt
          case ("com.geirsson", "scalafmt-cli_2.11", _, _)  => false
          case ("com.geirsson", "scalafmt-core_2.12", _, _) => false
          case _                                            => true
        }
    }) && (update.configurations.fold("")(_.toLowerCase) match {
      case "phantom-js-jetty"    => false
      case "scalafmt"            => false
      case "scripted-sbt"        => false
      case "scripted-sbt-launch" => false
      case "tut"                 => false
      case _                     => true
    })
    if (keep) Right(update) else Left(IgnoredGlobally(update))
  }

  def ignoreNonSnapshotToSnapshotUpdate(update: Update.Single): FilterResult = {
    val snap = "-SNAP"
    if (update.newerVersions.head.contains(snap) && !update.currentVersion.contains(snap))
      Left(NonSnapshotToSnapshotUpdate(update))
    else
      Right(update)
  }

  def removeBadVersions(update: Update.Single): FilterResult =
    util
      .removeAll(update.newerVersions, badVersions(update))
      .map(versions => update.copy(newerVersions = versions))
      .fold[FilterResult](Left(BadVersions(update)))(Right.apply)

  object ScalazVersions {
    def unapply(value: String): Boolean =
      (value.startsWith("7.3") || value.startsWith("8"))
  }

  private def badVersions(update: Update.Single): List[String] =
    (update.groupId, update.artifactId, update.currentVersion, update.nextVersion) match {
      // https://github.com/vlovgr/ciris/pull/182#issuecomment-420599759
      case ("com.jsuereth", "sbt-pgp", "1.1.2-1", "1.1.2") => List("1.1.2")

      case ("io.monix", _, _, _) =>
        List(
          // https://github.com/fthomas/scala-steward/issues/105
          "3.0.0-fbcb270"
        )
      case ("net.sourceforge.plantuml", "plantuml", _, _) =>
        List(
          // https://github.com/esamson/remder/pull/5
          "8059",
          // https://github.com/metabookmarks/sbt-plantuml-plugin/pull/10
          "2017.11"
        )
      case ("org.eclipse.jetty", _, _, _) =>
        List(
          "10.0.0-alpha0"
        )
      case ("org.http4s", _, _, _) =>
        List(
          // https://github.com/http4s/http4s/pull/2153
          "0.19.0"
        )
      case ("org.scalatest", "scalatest", _, _) =>
        List(
          // https://github.com/lightbend/migration-manager/pull/260
          "3.2.0-SNAP10"
        )
      case ("com.thesamet.scalapb", "protoc-bridge", _, _) =>
        List(
          // https://github.com/scalapb/protoc-bridge/issues/56
          "0.7.11"
        )
      case ("com.thesamet", "sbt-protoc", _, _) =>
        List(
          // https://github.com/scalapb/protoc-bridge/issues/56
          "0.99.26"
        )
      case ("org.scala-js", _, _, _) =>
        List(
          //https://github.com/scala-js/scala-js/issues/3865
          "0.6.30"
        )
      case _ => List.empty
    }
}
