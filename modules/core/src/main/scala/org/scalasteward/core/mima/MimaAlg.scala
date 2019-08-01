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
import cats.syntax.eq._
import cats.instances.string._
import coursier._
import io.chrisdavenport.log4cats.Logger
import com.typesafe.tools.mima.core
import com.typesafe.tools.mima.lib
import com.typesafe.tools.mima.core.util.log.Logging

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

trait MimaAlg[F[_]] {
  def backwardBinaryIssues(
      groupId: String,
      artifactId: String,
      current: String,
      newer: String
  ): F[String]
}

object MimaAlg {

  private case class Arg(groupId: String, artifactId: String, current: String, newer: String)

  private[this] val cache: TrieMap[Arg, String] = TrieMap.empty[Arg, String]

  def create[F[_]](
      implicit F: Monad[F],
      log: Logger[F]
  ): MimaAlg[F] =
    new MimaAlg[F] {
      override def backwardBinaryIssues(
          groupId: String,
          artifactId: String,
          current: String,
          newer: String
      ): F[String] = {
        val arg = Arg(
          groupId = groupId,
          artifactId = artifactId,
          current = current,
          newer = newer
        )
        F.point(
          cache.getOrElseUpdate(
            arg,
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
      ): String = {

        def fetch0(v: String, artifactId0: String): File =
          Fetch[coursier.util.Task]()
            .addDependencies(
              Dependency.of(
                Module(Organization(groupId), ModuleName(artifactId0)),
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
            fetch0(v, artifactId)
          } catch {
            case NonFatal(e0) =>
              println(e0)
              try {
                fetch0(v, artifactId + "_2.12")
              } catch {
                case NonFatal(e1) =>
                  println(e1)
                  try {
                    fetch0(v, artifactId + "_2.12_1.0")
                  } catch {
                    case NonFatal(e2) =>
                      println(e2)
                      throw e0
                  }
              }
          }

        try {
          val currentJar = fetch(current)
          val newJar = fetch(newer)

          val problems =
            makeMima(log).collectProblems(currentJar.getAbsolutePath, newJar.getAbsolutePath)

          val description = s"${groupId}:${artifactId}:${current} => ${newer}"

          val result = if (problems.isEmpty) {
            println("binary compatible!😊 " + description)
            ""
          } else {
            val e = problems
              .map(_.description(description))
              .sorted
              .mkString("```\n", "\n", "\n```")

            println(e)

            e
          }

          result
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            ""
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