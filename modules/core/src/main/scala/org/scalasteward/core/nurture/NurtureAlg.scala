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

package org.scalasteward.core.nurture

import cats.data.NonEmptyList
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.application.Config
import org.scalasteward.core.data.Update
import org.scalasteward.core.edit.EditAlg
import org.scalasteward.core.git.{Branch, GitAlg}
import org.scalasteward.core.mima.MimaAlg
import org.scalasteward.core.repoconfig.RepoConfigAlg
import org.scalasteward.core.data.Update
import org.scalasteward.core.model.UpdatesResult
import org.scalasteward.core.repoconfig.{RepoConfig, RepoConfigAlg}
import org.scalasteward.core.sbt.SbtAlg
import org.scalasteward.core.scalafmt.ScalafmtAlg
import org.scalasteward.core.update.FilterAlg
import org.scalasteward.core.util.{BracketThrowable, LogAlg}
import org.scalasteward.core.vcs.data.{NewPullRequestData, Repo}
import org.scalasteward.core.vcs.{VCSApiAlg, VCSRepoAlg}
import org.scalasteward.core.{git, util, vcs}

final class NurtureAlg[F[_]](
    implicit
    config: Config,
    editAlg: EditAlg[F],
    repoConfigAlg: RepoConfigAlg[F],
    filterAlg: FilterAlg[F],
    gitAlg: GitAlg[F],
    val vcsApiAlg: VCSApiAlg[F],
    vcsRepoAlg: VCSRepoAlg[F],
    logAlg: LogAlg[F],
    logger: Logger[F],
    pullRequestRepo: PullRequestRepository[F],
    sbtAlg: SbtAlg[F],
    scalafmtAlg: ScalafmtAlg[F],
    mima: MimaAlg[F],
    F: BracketThrowable[F]
) {
  def nurture(repo: Repo): F[Either[Throwable, Unit]] =
    logAlg.infoTotalTime(repo.show) {
      logAlg.attemptLog(util.string.lineLeftRight(s"Nurture ${repo.show}")) {
        for {
          (fork, baseBranch) <- cloneAndSync(repo)
          _ <- updateDependencies(repo, fork, baseBranch)
          _ <- gitAlg.removeClone(repo)
        } yield ()
      }
    }

  def cloneAndSync(repo: Repo): F[(Repo, Branch)] =
    for {
      _ <- logger.info(s"Clone and synchronize ${repo.show}")
      repoOut <- vcsApiAlg.createForkOrGetRepo(config, repo)
      _ <- vcsRepoAlg.clone(repo, repoOut)
      parent <- vcsRepoAlg.syncFork(repo, repoOut)
    } yield (repoOut.repo, parent.default_branch)

  private[this] def ignoreError[E](f: F[Unit]): F[Unit] =
    f.recoverWith {
      case e =>
        logger.info(e.toString)
    }

  def updateDependencies(repo: Repo, fork: Repo, baseBranch: Branch): F[Unit] =
    for {
      _ <- logger.info(s"Find updates for ${repo.show}")
      repoConfig <- repoConfigAlg.readRepoConfigOrDefault(repo)
      sbtUpdates <- sbtAlg.getUpdatesForRepo(repo)
      nonSbtUpdates <- getNonSbtUpdates(repo)
      updates = sbtUpdates ::: nonSbtUpdates
      filtered <- filterAlg
        .localFilterMany(repoConfig, updates)
        .map(
          _.filter(repo.filter)
        )
      grouped = Update.group(filtered)
      _ <- logger.info(util.logger.showUpdates(grouped))
      baseSha1 <- gitAlg.latestSha1(repo, baseBranch)
      _ <- {
        def deleteBranches(updates: List[UpdateData]): F[Unit] =
          updates.traverse_ { u =>
            ignoreError(gitAlg.deleteBranch(repo, u.updateBranch))
          }

        filtered match {
          case Nil =>
            F.unit
          case List(update) =>
            val data = UpdateData(
              repo,
              fork,
              RepoConfig.default,
              update,
              baseBranch,
              baseSha1,
              git.branchFor(update)
            )
            processUpdate(data)
          case h :: t =>
            for {
              res1 <- processUpdates(
                NonEmptyList(h, t).map { update =>
                  UpdateData(
                    repo,
                    fork,
                    RepoConfig.default,
                    update,
                    baseBranch,
                    baseSha1,
                    git.branchFor(update)
                  )
                }
              )
              _ <- F.whenA(res1.isFail) {
                for {
                  _ <- ignoreError(gitAlg.deleteBranch(repo, git.branchFor(filtered: _*)))
                  dataList = filtered.map { update =>
                    UpdateData(
                      repo,
                      fork,
                      RepoConfig.default,
                      update,
                      baseBranch,
                      baseSha1,
                      git.branchFor(update)
                    )
                  }
                  _ <- {
                    if (filtered.size === 2) {
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

  def getNonSbtUpdates(repo: Repo): F[List[Update.Single]] =
    for {
      maybeScalafmt <- scalafmtAlg.getScalafmtUpdate(repo)
    } yield List(maybeScalafmt).flatten

  def processUpdates(
      data: NonEmptyList[UpdateData]
  ): F[UpdatesResult] =
    for {
      _ <- logger.info(s"Process updates ${data.map(_.update.show)}")
      b = git.branchFor(data.toList.map(_.update): _*)
      res <- (vcsApiAlg.getBranch(data.head.repo, b) >> F.point(UpdatesResult.skip))
        .recoverWith {
          case e =>
            logger.info(e.toString) >> applyNewUpdates(data)
        }
    } yield res

  def remoteBranchExists[E](data: UpdateData): F[Boolean] = {
    val branchName = git.branchFor(data.update)
    remoteBranchExists(data.repo, branchName)
  }

  def remoteBranchExists[E](repo: Repo, branch: Branch): F[Boolean] =
    vcsApiAlg
      .getBranch(repo.copy(owner = config.vcsLogin), branch)
      .flatMap { res =>
        logger.info(s"$res ${repo.show} $branch") >> F.point(true)
      }
      .recoverWith {
        case e =>
          logger.info(s"$e ${repo.show} $branch") >> F.point(false)
      }

  def processUpdate(data: UpdateData): F[Unit] =
    for {
      _ <- logger.info(s"Process update ${data.update.show}")
      head = vcs.listingBranch(config.vcsType, data.fork, data.update)
      branchName = git.branchFor(data.update)
      exists <- remoteBranchExists(data)
      _ <- if (exists) {
        logger.info(s"Found the branch in remote. skip ${data.repo.show} ${data.update.show}")
      } else {
        for {
          pullRequests <- vcsApiAlg.listPullRequests(data.repo, head, data.baseBranch)
          _ <- pullRequests.headOption match {
            case Some(pr) if pr.isClosed =>
              logger.info(s"PR ${pr.html_url} is closed")
            case _ =>
              applyNewUpdate(data)
          }
        } yield ()
      }
    } yield ()

  def applyNewUpdates(data: NonEmptyList[UpdateData]): F[UpdatesResult] = {
    val d = data.head
    val repo = d.repo
    for {
      result <- editAlg.applyUpdates(repo, data.map(_.update)).map(_.toSet)
      filtered = data.map(_.update).filter(result)
      s <- gitAlg
        .containsChanges(repo)
        .ifM(
          gitAlg.returnToCurrentBranch(repo) {
            val branch = git.branchFor(filtered: _*)
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
                  binaryIssues <- filtered
                    .traverse { update =>
                      mima.backwardBinaryIssues(
                        groupId = update.groupId,
                        artifactId = update.artifactId,
                        current = update.currentVersion,
                        newer = update.newerVersions.head
                      )
                    }
                    .map(_.mkString("\n\n"))
                  message = filtered match {
                    case List(x) =>
                      git.commitMsgFor(x)
                    case _ =>
                      "Update dependencies"
                  }
                  s <- commitAndPush(repo, message + "\n\n" + binaryIssues, branch)
                  _ <- if (s) {
                    createPullRequest(
                      baseBranch = d.baseBranch,
                      repo = repo,
                      fork = data.head.fork,
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

  def applyNewUpdate(data: UpdateData): F[Boolean] =
    for {
      success <- applyAndCheck(data)
      _ <- {
        if (success) {
          for {
            _ <- gitAlg.push(data.repo, data.updateBranch, force = false)
            _ <- createPullRequest(
              baseBranch = data.baseBranch,
              repo = data.repo,
              fork = data.fork,
              branch = data.updateBranch,
              message = git.commitMsgFor(data.update)
            )
          } yield ()
        } else F.unit
      }
    } yield success

  def commitAndPush(repo: Repo, message: String, branch: Branch): F[Boolean] =
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

  def commitAndPush(data: UpdateData): F[Unit] =
    for {
      _ <- logger.info("Commit and push changes")
      binaryIssues <- mima.backwardBinaryIssues(
        groupId = data.update.groupId,
        artifactId = data.update.artifactId,
        current = data.update.currentVersion,
        newer = data.update.newerVersions.head
      )
      _ <- gitAlg.commitAll(data.repo, git.commitMsgFor(data.update) + "\n\n" + binaryIssues)
      _ <- gitAlg.push(data.repo, data.updateBranch, force = false)
    } yield ()

  def createPullRequest(
      baseBranch: Branch,
      repo: Repo,
      fork: Repo,
      branch: Branch,
      message: String
  ): F[Unit] =
    if (repo.createPullRequest) {
      for {
        _ <- logger.info(s"Create PR ${branch.name}")
        requestData = NewPullRequestData(
          title = message,
          body = "",
          head = s"${fork.owner}:${branch.name}",
          base = baseBranch
        )
        pullRequest <- vcsApiAlg.createPullRequest(repo, requestData)
        _ <- logger.info(s"Created PR ${pullRequest.html_url}")
      } yield ()
    } else {
      logger.info(s"skip create PR for ${repo.show}")
    }

  def createPullRequest(data: UpdateData): F[Unit] =
    for {
      _ <- logger.info(s"Create PR ${data.updateBranch.name}")
      branchName = vcs.createBranch(config.vcsType, data.fork, data.update)
      binaryIssues <- mima.backwardBinaryIssues(
        groupId = data.update.groupId,
        artifactId = data.update.artifactId,
        current = data.update.currentVersion,
        newer = data.update.newerVersions.head
      )
      requestData = NewPullRequestData.from(data, branchName, config.vcsLogin, binaryIssues)
      pr <- vcsApiAlg.createPullRequest(data.repo, requestData)
      _ <- pullRequestRepo.createOrUpdate(
        data.repo,
        pr.html_url,
        data.baseSha1,
        data.update,
        pr.state
      )
      _ <- logger.info(s"Created PR ${pr.html_url}")
    } yield ()

  def updatePullRequest(data: UpdateData): F[Unit] =
    gitAlg.returnToCurrentBranch(data.repo) {
      for {
        _ <- gitAlg.checkoutBranch(data.repo, data.updateBranch)
        updated <- shouldBeUpdated(data)
        _ <- if (updated) mergeAndApplyAgain(data) else F.unit
      } yield ()
    }

  def shouldBeUpdated(data: UpdateData): F[Boolean] = {
    val result = gitAlg.isMerged(data.repo, data.updateBranch, data.baseBranch).flatMap {
      case true => (false, "PR has been merged").pure[F]
      case false =>
        gitAlg.branchAuthors(data.repo, data.updateBranch, data.baseBranch).flatMap { authors =>
          val distinctAuthors = authors.distinct
          if (distinctAuthors.length >= 2)
            (false, s"PR has commits by ${distinctAuthors.mkString(", ")}").pure[F]
          else
            gitAlg.hasConflicts(data.repo, data.updateBranch, data.baseBranch).map {
              case true  => (true, s"PR has conflicts with ${data.baseBranch.name}")
              case false => (false, s"PR has no conflict with ${data.baseBranch.name}")
            }
        }
    }
    result.flatMap { case (reset, msg) => logger.info(msg).as(reset) }
  }

  def mergeAndApplyAgain(data: UpdateData): F[Unit] =
    for {
      _ <- logger.info(
        s"Merge branch '${data.baseBranch.name}' into ${data.updateBranch.name} and apply again"
      )
      _ <- gitAlg.mergeTheirs(data.repo, data.baseBranch)
      _ <- editAlg.applyUpdate(data.repo, data.update)
      containsChanges <- gitAlg.containsChanges(data.repo)
      _ <- if (containsChanges) commitAndPush(data) else F.unit
    } yield ()

  def applyAndCheck(data: UpdateData): F[Boolean] =
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

  def commitAndCheck(data: UpdateData): F[Boolean] =
    for {
      binaryIssues <- mima.backwardBinaryIssues(
        groupId = data.update.groupId,
        artifactId = data.update.artifactId,
        current = data.update.currentVersion,
        newer = data.update.newerVersions.head
      )
      _ <- gitAlg.commitAll(data.repo, git.commitMsgFor(data.update) + "\n\n" + binaryIssues)
      success <- sbtAlg.run(data.repo).map(_ => true).recoverWith {
        case e =>
          logger.error(e)(s"sbt ${data.repo.testCommands.mkString(" ")} fail") >> F.point(false)
      }
    } yield success
}
