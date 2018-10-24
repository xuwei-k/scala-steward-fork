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
          _ <- repos.traverse_(ctx.nurtureAlg.nurture)
        } yield ExitCode.Success
      }
    }

  def prepareEnv(ctx: Context[IO]): IO[Unit] =
    for {
      _ <- ctx.sbtAlg.addGlobalPlugins
      _ <- ctx.workspaceAlg.cleanWorkspace
    } yield ()

  def getRepos(repos: List[String]): List[Repo] = {
    repos.map { name =>
      val Array(user, repo) = name.split('/')
      Repo(user, repo)
    }
  }
}
