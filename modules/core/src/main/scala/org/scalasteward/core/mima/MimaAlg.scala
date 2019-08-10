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

package org.scalasteward.core.mima

import java.io.File

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.eq._
import cats.instances.string._
import coursier._
import io.chrisdavenport.log4cats.Logger
import com.typesafe.tools.mima.core
import com.typesafe.tools.mima.lib
import com.typesafe.tools.mima.core.util.log.Logging

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

sealed abstract class MimaResult {
  def simple: String =
    this match {
      case MimaResult.Imcompatible(values, lib) =>
        s"""`"${lib.groupId}" % "${lib.artifactId}" % "${lib.current}" => "${lib.newer}"` found ${values.size} binary incompatibilities"""
      case _ =>
        ""
    }
  def full: String =
    this match {
      case MimaResult.Imcompatible(values, _) =>
        simple + "\n\n" + values.sorted.toList.mkString("```\n", "\n", "\n```")
      case _ =>
        ""
    }
  def asString(limit: Int): String =
    this match {
      case MimaResult.Imcompatible(_, _) =>
        if (full.length > limit) {
          simple
        } else {
          full
        }
      case _ =>
        ""
    }
}
object MimaResult {
  case object Compatible extends MimaResult
  final case class Imcompatible(values: NonEmptyList[String], lib: Lib) extends MimaResult
  final case class Err(err: Throwable) extends MimaResult
}

trait MimaAlg[F[_]] {
  def backwardBinaryIssues(
      groupId: String,
      artifactId: String,
      current: String,
      newer: String
  ): F[MimaResult]

  def backwardBinaryIssuesString(
      groupId: String,
      artifactId: String,
      current: String,
      newer: String
  ): F[String]
}

case class Lib(groupId: String, artifactId: String, current: String, newer: String)

object MimaAlg {
  private[this] val cache: TrieMap[Lib, MimaResult] = TrieMap.empty[Lib, MimaResult]

  def defaultLimit = 2048

  def create[F[_]](
      implicit F: Monad[F],
      log: Logger[F]
  ): MimaAlg[F] =
    new MimaAlg[F] {

      override def backwardBinaryIssuesString(
          groupId: String,
          artifactId: String,
          current: String,
          newer: String
      ): F[String] =
        F.map(backwardBinaryIssues(groupId, artifactId, current, newer))(_.asString(defaultLimit))

      override def backwardBinaryIssues(
          groupId: String,
          artifactId: String,
          current: String,
          newer: String
      ): F[MimaResult] = {
        val lib = Lib(
          groupId = groupId,
          artifactId = artifactId,
          current = current,
          newer = newer
        )
        F.point(
          cache.getOrElseUpdate(
            lib,
            backwardBinaryIssues0(
              groupId = groupId,
              artifactId = artifactId,
              current = current,
              newer = newer
            )
          )
        )
      }

      private[this] def backwardBinaryIssues0(
          groupId: String,
          artifactId: String,
          current: String,
          newer: String
      ): MimaResult = {

        def fetch0(v: String, artifactId0: String, attributes: Map[String, String]): File =
          Fetch[coursier.util.Task]()
            .addDependencies(
              Dependency.of(
                Module(Organization(groupId), ModuleName(artifactId0), attributes),
                v
              )
            )
            .withRepositories(
              Seq(
                Repositories.central,
                Repositories.sbtPlugin("releases")
              )
            )
            .run()
            .filter(_.getName === s"${artifactId0}-${v}.jar") match {
            case Seq()    => sys.error("could not found jar")
            case Seq(jar) => jar
            case xs       => sys.error(s"found multiple jars!? ${xs}")
          }

        def fetch(v: String): File =
          try {
            fetch0(v, artifactId, Map.empty)
          } catch {
            case NonFatal(e0) =>
              println(e0)
              try {
                fetch0(v, artifactId + "_2.12", Map.empty)
              } catch {
                case NonFatal(e1) =>
                  println(e1)
                  try {
                    fetch0(v, artifactId, Map("scalaVersion" -> "2.12", "sbtVersion" -> "1.0"))
                  } catch {
                    case NonFatal(e2) =>
                      println(e2)
                      try {
                        fetch0(v, artifactId, Map("scalaVersion" -> "2.10", "sbtVersion" -> "0.13"))
                      } catch {
                        case NonFatal(e3) =>
                          println(e3)
                          throw e0
                      }
                  }
              }
          }

        try {
          val currentJar = fetch(current)
          val newJar = fetch(newer)

          val problems =
            makeMima(log).collectProblems(currentJar.getAbsolutePath, newJar.getAbsolutePath)

          val description = s"${groupId}:${artifactId}:${current} => ${newer}"

          problems match {
            case Nil =>
              println("binary compatible!😊 " + description)
              MimaResult.Compatible
            case x :: xs =>
              val res = MimaResult.Imcompatible(
                NonEmptyList.of(x, xs: _*).map(_.description(description)),
                Lib(
                  groupId = groupId,
                  artifactId = artifactId,
                  current = current,
                  newer = newer
                )
              )
              println(res.asString(Int.MaxValue))
              res
          }
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            MimaResult.Err(e)
        }
      }
    }

  private def makeMima[F[_]](log: Logger[F]): lib.MiMaLib = {
    core.Config.setup("scala-steward", Array.empty)
    val classpath = com.typesafe.tools.mima.core.reporterClassPath("")
    new lib.MiMaLib(classpath, new MimaLogger(log))
  }

  private[this] class MimaLogger[F[_]](log: Logger[F]) extends Logging {
    override def debugLog(str: String): Unit = {
      val _ = log.debug(str)
    }
    override def error(str: String): Unit = {
      val _ = log.error(str)
    }
    override def info(str: String): Unit = {
      val _ = log.info(str)
    }
    override def warn(str: String): Unit = {
      val _ = log.warn(str)
    }
  }

}
