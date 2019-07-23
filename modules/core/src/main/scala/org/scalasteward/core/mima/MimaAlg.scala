package org.scalasteward.core.mima

import cats.Monad
import cats.syntax.eq._
import cats.instances.string._
import coursier._
import io.chrisdavenport.log4cats.Logger
import com.typesafe.tools.mima.core
import com.typesafe.tools.mima.lib
import com.typesafe.tools.mima.core.util.log.Logging

trait MimaAlg[F[_]] {
  def backwordBinaryIssues(
      groupId: String,
      artifactId: String,
      current: String,
      newer: String
  ): F[String]
}

object MimaAlg {
  def create[F[_]](
      implicit F: Monad[F],
      log: Logger[F]
  ): MimaAlg[F] =
    new MimaAlg[F] {
      override def backwordBinaryIssues(
          groupId: String,
          artifactId: String,
          current: String,
          newer: String
      ): F[String] = {
        val currentFiles = Fetch[coursier.util.Task]()
          .addDependencies(
            Dependency.of(
              Module(Organization(groupId), ModuleName(artifactId)),
              current
            )
          )
          .run()

        val newFiles = Fetch[coursier.util.Task]()
          .addDependencies(
            Dependency.of(
              Module(Organization(groupId), ModuleName(artifactId)),
              newer
            )
          )
          .run()

        val currentJar = currentFiles.filter(_.getName === s"${artifactId}-${current}.jar") match {
          case Seq()    => sys.error("could not found jar")
          case Seq(jar) => jar
          case xs       => sys.error(s"found multiple jars!? ${xs}")
        }

        val newJar = newFiles.filter(_.getName === s"${artifactId}-${newer}.jar") match {
          case Seq()    => sys.error("could not found jar")
          case Seq(jar) => jar
          case xs       => sys.error(s"found multiple jars!? ${xs}")
        }

        val problems =
          makeMima(log).collectProblems(currentJar.getAbsolutePath, newJar.getAbsolutePath)

        val result = if (problems.isEmpty) {
          "Found 0 binary incompatibilities"
        } else {
          problems
            .map(_.description(s"${groupId}:${artifactId}:${current} => ${newer}"))
            .sorted
            .mkString("\n")
        }

        F.point(result)
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
