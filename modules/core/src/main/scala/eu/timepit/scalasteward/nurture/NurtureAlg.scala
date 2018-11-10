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

package eu.timepit.scalasteward.nurture

import cats.effect.Sync
import cats.implicits._
import cats.{ApplicativeError, FlatMap, MonadError}
import cats.data.NonEmptyList
import eu.timepit.scalasteward.application.Config
import eu.timepit.scalasteward.git.{Branch, GitAlg}
import eu.timepit.scalasteward.github.GitHubApiAlg
import eu.timepit.scalasteward.github.data.{AuthenticatedUser, NewPullRequestData, Repo}
import eu.timepit.scalasteward.model.UpdatesResult
import eu.timepit.scalasteward.sbt.SbtAlg
import eu.timepit.scalasteward.update.FilterAlg
import eu.timepit.scalasteward.util.logger.LoggerOps
import eu.timepit.scalasteward.util.{BracketThrowable, MonadThrowable}
import eu.timepit.scalasteward.{git, github, util}
import io.chrisdavenport.log4cats.Logger

class NurtureAlg[F[_]](
    implicit
    config: Config,
    editAlg: EditAlg[F],
    filterAlg: FilterAlg[F],
    gitAlg: GitAlg[F],
    gitHubApiAlg: GitHubApiAlg[F],
    logger: Logger[F],
    sbtAlg: SbtAlg[F],
    user: AuthenticatedUser
) {
  def nurture(repo: Repo)(implicit F: Sync[F]): F[Unit] =
    logger.infoTotalTime(repo.show) {
      logger.attemptLog_(s"Nurture ${repo.show}") {
        for {
          baseBranch <- cloneAndSync(repo)
          _ <- updateDependencies(repo, baseBranch)
          _ <- gitAlg.removeClone(repo)
        } yield ()
      }
    }

  def cloneAndSync(repo: Repo)(implicit F: MonadThrowable[F]): F[Branch] =
    for {
      _ <- logger.info(s"Clone and synchronize ${repo.show}")
      repoOut <- gitHubApiAlg.createFork(repo)
      parent <- repoOut.parentOrRaise[F]
      cloneUrl = util.uri.withUserInfo(repoOut.clone_url, user)
      parentCloneUrl = util.uri.withUserInfo(parent.clone_url, user)
      _ <- gitAlg.clone(repo, cloneUrl)
      _ <- gitAlg.setAuthor(repo, config.gitAuthor)
      _ <- gitAlg.syncFork(repo, parentCloneUrl, parent.default_branch)
    } yield parent.default_branch

  private[this] def ignoreError[E](f: F[Unit])(implicit F: ApplicativeError[F, E]): F[Unit] =
    f.recoverWith {
      case e =>
        logger.info(e.toString)
    }

  def updateDependencies(repo: Repo, baseBranch: Branch)(implicit F: BracketThrowable[F]): F[Unit] =
    for {
      _ <- logger.info(s"Check updates for ${repo.show}")
      updates <- sbtAlg.getUpdatesForRepo(repo)
      _ <- logger.info(util.logger.showUpdates(updates))
      filtered <- filterAlg.localFilterMany(repo, updates)
      _ <- {
        def deleteBranches(updates: List[UpdateData]): F[Unit] =
          updates.traverse_ { u =>
            ignoreError(gitAlg.deleteBranch(repo, u.updateBranch))
          }

        filtered match {
          case Nil =>
            F.unit
          case List(update) =>
            val data = UpdateData(repo, update, baseBranch, git.branchFor(update))
            processUpdate(data)
          case h :: t =>
            for {
              res1 <- processUpdates(
                NonEmptyList(h, t).map { update =>
                  UpdateData(repo, update, baseBranch, git.branchFor(update))
                }
              )
              _ <- F.whenA(res1.isFail) {
                for {
                  _ <- ignoreError(gitAlg.deleteBranch(repo, git.branchFor(filtered: _*)))
                  dataList = filtered.map { update =>
                    UpdateData(repo, update, baseBranch, git.branchFor(update))
                  }
                  _ <- {
                    if (filtered.size == 2) {
                      dataList.traverse_ { data =>
                        ignoreError(processUpdate(data))
                      }
                    } else {
                      for {
                        successUpdates <- dataList.filterA(applyAndCheck)
                        _ <- deleteBranches(successUpdates)
                        _ <- successUpdates match {
                          case Nil =>
                            F.unit
                          case x :: Nil =>
                            processUpdate(x)
                          case x :: xs =>
                            processUpdates(NonEmptyList(x, xs))
                        }
                      } yield ()
                    }
                  }
                } yield ()
              }
            } yield ()
        }
      }
    } yield ()

  def processUpdates(
      data: NonEmptyList[UpdateData]
  )(implicit F: BracketThrowable[F]): F[UpdatesResult] =
    for {
      _ <- logger.info(s"Process updates ${data.map(_.update.show)}")
      b = git.branchFor(data.toList.map(_.update): _*)
      res <- (gitHubApiAlg.getBranch(data.head.repo, b) >> F.point(UpdatesResult.skip))
        .recoverWith {
          case e =>
            logger.info(e.toString) >> applyNewUpdates(data)
        }
    } yield res

  def processUpdate(data: UpdateData)(implicit F: BracketThrowable[F]): F[Unit] =
    for {
      _ <- logger.info(s"Process update ${data.update.show}")
      head = github.headFor(config.gitHubLogin, data.update)
      branchName = git.branchFor(data.update)
      exists <- remoteBranchExists(data)
      _ <- if (exists) {
        logger.info(s"Found the branch in remote. skip ${data.repo.show} ${data.update.show}")
      } else {
        for {
          pullRequests <- gitHubApiAlg.listPullRequests(data.repo, head)
          _ <- pullRequests.headOption match {
            case Some(pr) if pr.isClosed =>
              logger.info(s"PR ${pr.html_url} is closed")
            case Some(pr) =>
              logger.info(s"Found PR ${pr.html_url}") >> updatePullRequest(data)
            case None =>
              applyNewUpdate(data)
          }
        } yield ()
      }
    } yield ()

  def applyNewUpdates(
      data: NonEmptyList[UpdateData]
  )(implicit F: BracketThrowable[F]): F[UpdatesResult] = {
    val d = data.head
    val repo = d.repo
    for {
      result <- editAlg.applyUpdates(repo, data.map(_.update)).map(_.toSet)
      s <- gitAlg
        .containsChanges(repo)
        .ifM(
          gitAlg.returnToCurrentBranch(repo) {
            val branch = git.branchFor(data.map(_.update).filterNot(result): _*)
            val repo = data.head.repo
            for {
              exists <- remoteBranchExists(repo, branch)
              res <- if (exists) {
                logger.info(s"Found the branch in remote. skip ${repo.show} ${branch}") >> F
                  .point(UpdatesResult.skip)
              } else {
                for {
                  _ <- logger.info(s"Create branch ${branch.name}")
                  _ <- gitAlg.createBranch(repo, branch)
                  message = "Update dependencies"
                  s <- commitAndPush(repo, message, branch)
                  _ <- if (s) {
                    createPullRequest(
                      baseBranch = d.baseBranch,
                      repo = repo,
                      branch = branch,
                      message = message
                    )
                  } else {
                    F.unit
                  }
                } yield {
                  if (s) UpdatesResult.doUpdate else UpdatesResult.failure
                }
              }
            } yield res
          }, {
            logger.warn("No files were changed") >> F.point(UpdatesResult.skip)
          }
        )
    } yield s
  }

  def applyNewUpdate(data: UpdateData)(implicit F: BracketThrowable[F]): F[Boolean] =
    for {
      success <- applyAndCheck(data)
      _ <- {
        if (success) {
          for {
            _ <- pushToGitHub(data)
            _ <- createPullRequest(
              baseBranch = data.baseBranch,
              repo = data.repo,
              branch = data.updateBranch,
              message = git.commitMsgFor(data.update)
            )
          } yield ()
        } else F.unit
      }
    } yield success

  def remoteBranchExists[E](data: UpdateData)(implicit F: MonadError[F, E]): F[Boolean] = {
    val branchName = git.branchFor(data.update)
    remoteBranchExists(data.repo, branchName)
  }

  def remoteBranchExists[E](repo: Repo, branch: Branch)(
      implicit F: MonadError[F, E]
  ): F[Boolean] =
    gitHubApiAlg
      .getBranch(repo.copy(owner = config.gitHubLogin), branch)
      .flatMap { res =>
        logger.info(s"$res ${repo.show} $branch") >> F.point(true)
      }
      .recoverWith {
        case e =>
          logger.info(s"$e ${repo.show} $branch") >> F.point(false)
      }

  def applyAndCheck(data: UpdateData)(implicit F: BracketThrowable[F]): F[Boolean] =
    for {
      exists <- remoteBranchExists(data)
      s <- if (exists) {
        logger.info(s"Found the branch in remote. skip ${data.repo.show} ${data.update.show}") >> F
          .point(false)
      } else {
        (editAlg.applyUpdate(data.repo, data.update) >> gitAlg.containsChanges(data.repo)).ifM(
          gitAlg.returnToCurrentBranch(data.repo) {
            for {
              _ <- logger.info(s"Create branch ${data.updateBranch.name}")
              _ <- gitAlg.createBranch(data.repo, data.updateBranch)
              success <- commitAndCheck(data)
            } yield success
          },
          logger.warn("No files were changed") >> F.point(false)
        )
      }
    } yield s

  def commitAndPush(repo: Repo, message: String, branch: Branch)(
      implicit F: MonadThrowable[F]
  ): F[Boolean] =
    for {
      _ <- gitAlg.commitAll(repo, message)
      success <- sbtAlg.run(repo).map(_ => true).recoverWith {
        case e =>
          logger.error(e)(s"failed sbt ${repo.testCommands.mkString(" ")}") >> F.point(false)
      }
      _ <- F.whenA(success) {
        gitAlg.push(repo, branch, force = false)
      }
    } yield success

  def commitAndPush(data: UpdateData)(implicit F: MonadThrowable[F]): F[Unit] =
    commitAndCheck(data).ifM(pushToGitHub(data), F.unit)

  def pushToGitHub(data: UpdateData): F[Unit] =
    gitAlg.push(data.repo, data.updateBranch, force = false)

  def commitAndCheck(data: UpdateData)(implicit F: MonadThrowable[F]): F[Boolean] =
    for {
      _ <- gitAlg.commitAll(data.repo, git.commitMsgFor(data.update))
      success <- sbtAlg.run(data.repo).map(_ => true).recoverWith {
        case e =>
          logger.error(e)(s"sbt ${data.repo.testCommands.mkString(" ")} fail") >> F.point(false)
      }
    } yield success

  def createPullRequest(baseBranch: Branch, repo: Repo, branch: Branch, message: String)(
      implicit F: FlatMap[F]
  ): F[Unit] =
    if (repo.createPullRequest) {
      for {
        _ <- logger.info(s"Create PR ${branch.name}")
        requestData = NewPullRequestData.from(
          message = message,
          headBranch = branch,
          baseBranch = baseBranch,
          login = config.gitHubLogin
        )
        pullRequest <- gitHubApiAlg.createPullRequest(repo, requestData)
        _ <- logger.info(s"Created PR ${pullRequest.html_url}")
      } yield ()
    } else {
      logger.info(s"skip create PR for ${repo.show}")
    }

  def updatePullRequest(data: UpdateData)(implicit F: BracketThrowable[F]): F[Unit] =
    gitAlg.returnToCurrentBranch(data.repo) {
      for {
        _ <- gitAlg.checkoutBranch(data.repo, data.updateBranch)
        reset <- shouldBeReset(data)
        _ <- if (reset) resetAndUpdate(data) else F.unit
      } yield ()
    }

  def shouldBeReset(data: UpdateData)(implicit F: FlatMap[F]): F[Boolean] =
    for {
      authors <- gitAlg.branchAuthors(data.repo, data.updateBranch, data.baseBranch)
      distinctAuthors = authors.distinct
      isBehind <- gitAlg.isBehind(data.repo, data.updateBranch, data.baseBranch)
      isMerged <- gitAlg.isMerged(data.repo, data.updateBranch, data.baseBranch)
      (result, msg) = {
        if (isMerged)
          (false, "PR has been merged")
        else if (distinctAuthors.length >= 2)
          (false, s"PR has commits by ${distinctAuthors.mkString(", ")}")
        else if (authors.length >= 2)
          (true, "PR has multiple commits")
        else if (isBehind)
          (true, s"PR is behind ${data.baseBranch.name}")
        else
          (false, s"PR is up-to-date with ${data.baseBranch.name}")
      }
      _ <- logger.info(msg)
    } yield result

  def resetAndUpdate(data: UpdateData)(implicit F: MonadThrowable[F]): F[Unit] =
    for {
      _ <- logger.info(s"Reset and update ${data.updateBranch.name}")
      _ <- gitAlg.resetHard(data.repo, data.baseBranch)
      _ <- editAlg.applyUpdate(data.repo, data.update)
      containsChanges <- gitAlg.containsChanges(data.repo)
      _ <- if (containsChanges) commitAndPush(data) else F.unit
    } yield ()
}
