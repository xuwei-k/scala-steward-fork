/*
 * Copyright 2018 scala-steward contributors
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

package eu.timepit.scalasteward

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import eu.timepit.scalasteward.application.Context
import eu.timepit.scalasteward.github.data.Repo
import eu.timepit.scalasteward.util.logger.LoggerOps

object steward extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    Context.create[IO].use { ctx =>
      ctx.logger.infoTotalTime {
        val repos = getRepos(args)

        for {
          _ <- prepareEnv(ctx)
          //user <- ctx.config.gitHubUser[IO]
          //_ <- repos.traverse(ctx.dependencyService.forkAndCheckDependencies(user, _))
          //_ <- ctx.updateService.checkForUpdates
          _ <- repos.traverse_(ctx.gitHubApiAlg.createFork)
          _ = Thread.sleep(5000)
          _ <- repos.traverse_(ctx.nurtureAlg.nurture)
        } yield ExitCode.Success
      }
    }

  def prepareEnv(ctx: Context[IO]): IO[Unit] =
    for {
      _ <- ctx.sbtAlg.addGlobalPlugins
      _ <- ctx.workspaceAlg.cleanWorkspace
    } yield ()

  def getRepos(repos: List[String]): List[Repo] =
    if (repos.isEmpty) {
      List(
        Repo("xuwei-k", "scalikejdbc-flyway-sbt-example"),
        Repo("xuwei-k", "mima-web"),
        Repo("xuwei-k", "favorite_typo"),
        Repo("xuwei-k", "applybuilder"),
        Repo("xuwei-k", "qiitascala"),
        Repo("xuwei-k", "qiita-twitter-bot"),
        Repo("xuwei-k", "play-json-extra"),
        Repo("xuwei-k", "httpz"),
        Repo("xuwei-k", "webpush-scala"),
        Repo("xuwei-k", "githubtree"),
        Repo("xuwei-k", "discourse-bot"),
        Repo("xuwei-k", "httpmock"),
        Repo("xuwei-k", "msgpack-json"),
        Repo("xuwei-k", "java-src"),
        Repo("xuwei-k", "javadoc-badge"),
        Repo("xuwei-k", "scalaz-magnolia"),
        Repo("xuwei-k", "iarray"),
        Repo("xuwei-k", "githubot"),
        Repo("xuwei-k", "play2scalaz"),
        Repo("xuwei-k", "mima"),
        Repo("xuwei-k", "sonatype"),
        Repo("xuwei-k", "optparse-applicative"),
        Repo("xuwei-k", "sbt-jshell"),
        Repo("xuwei-k", "zeroapply"),
        Repo("xuwei-k", "sbt-class-diagram"),
        Repo("xuwei-k", "nobox"),
        Repo("xuwei-k", "sbt-conflict-classes"),
        Repo("xuwei-k", "scalajspack"),
        Repo("xuwei-k", "gitter-scala"),
        Repo("scalaprops", "scalaprops"),
        Repo("scalaprops", "scalaprops-native-example"),
        Repo("scalaprops", "scalaprops-examples"),
        Repo("scalaprops", "scalaprops-cross-example"),
        Repo("scalaprops", "scalaprops-magnolia"),
        Repo("scalaprops", "scalaprops-shapeless"),
        Repo("scalaprops", "sbt-scalaprops"),
        Repo("msgpack4z", "msgpack4z-core"),
        Repo("msgpack4z", "msgpack4z-native"),
        Repo("msgpack4z", "msgpack4z-circe"),
        Repo("msgpack4z", "msgpack4z-argonaut"),
        Repo("msgpack4z", "msgpack4z-play"),
        Repo("msgpack4z", "msgpack4z-jawn"),
        Repo("msgpack4z", "msgpack4z-native"),
        Repo("scalapb-json", "scalapb-playjson"),
        Repo("scalapb-json", "scalapb-circe"),
        Repo("scalapb-json", "scalapb-argonaut"),
        Repo("scalapb-json", "protoc-lint"),
        Repo("scalapb-json", "scalapb-json-common")
      )
    } else {
      repos.map { name =>
        val Array(user, repo) = name.split('/')
        Repo(user, repo)
      }
    }
}
