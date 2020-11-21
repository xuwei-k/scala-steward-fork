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

package org.scalasteward.core.application

import caseapp._
import caseapp.core.Error.MalformedValue
import caseapp.core.argparser.{ArgParser, SimpleArgParser}
import cats.implicits._
import org.http4s.{Http4sLiteralSyntax, Uri}
import org.scalasteward.core.application.Cli._
import org.scalasteward.core.util.ApplicativeThrowable

final class Cli[F[_]](implicit F: ApplicativeThrowable[F]) {
  def parseArgs(args: List[String]): F[Args] =
    F.fromEither {
      CaseApp.parse[Args](args).bimap(e => new Throwable(e.message), { case (parsed, _) => parsed })
    }
}

object Cli {
  final case class Args(
      workspace: String = "scala-steward-workspace",
      reposFile: String = "dummy",
      gitAuthorName: String = "xuwei-k-bot[bot]",
      gitAuthorEmail: String = "74788111+xuwei-k-bot[bot]@users.noreply.github.com",
      vcsType: SupportedVCS = SupportedVCS.GitHub,
      vcsApiHost: Uri = uri"https://api.github.com",
      vcsLogin: String = "xuwei-k-bot[bot]",
      gitAskPass: String = "dummy git ask pass",
      signCommits: Boolean = false,
      whitelist: List[String] = Nil,
      readOnly: List[String] = Nil,
      disableSandbox: Boolean = true,
      doNotFork: Boolean = false,
      ignoreOptsFiles: Boolean = true,
      keepCredentials: Boolean = false,
      envVar: List[EnvVar] = Nil,
      pruneRepos: Boolean = false
  )

  final case class EnvVar(name: String, value: String)

  implicit val envVarParser: SimpleArgParser[EnvVar] =
    SimpleArgParser.from[EnvVar]("env-var") { s =>
      s.trim.split('=').toList match {
        case name :: value :: Nil =>
          Right(EnvVar(name.trim, value.trim))
        case _ =>
          Left(
            MalformedValue("EnvVar", "The value is expected in the following format: NAME=VALUE.")
          )
      }
    }

  implicit val uriArgParser: ArgParser[Uri] =
    ArgParser[String].xmapError(
      _.renderString,
      s => Uri.fromString(s).leftMap(pf => MalformedValue("Uri", pf.message))
    )
}
