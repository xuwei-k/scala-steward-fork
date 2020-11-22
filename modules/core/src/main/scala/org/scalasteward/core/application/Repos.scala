package org.scalasteward.core.application

import org.scalasteward.core.vcs.data.Repo
import cats.implicits._
import scala.util.Random

object Repos {

  object RunPartial {
    object UInt {
      def unapply(value: String): Option[Int] =
        try {
          Some(value.toInt)
        } catch {
          case _: NumberFormatException =>
            None
        }
    }
    def unapply(value: String): Option[(Int, Int)] =
      PartialFunction.condOpt(value.split('-')) {
        case Array(UInt(x), UInt(y)) =>
          (x, y)
      }
  }

  val defaultRepos = List(
    Repo("xuwei-k", "replace-symbol-literals"),
    Repo("xuwei-k", "play-ws-scalafix"),
    Repo("xuwei-k", "scalaz-docs"),
    Repo("xuwei-k", "scala-protobuf-docs"),
    Repo("xuwei-k", "scalikejdbc-flyway-sbt-example"),
    Repo("xuwei-k", "mima-web"),
    Repo("xuwei-k", "favorite_typo"),
    Repo("xuwei-k", "applybuilder"),
    Repo("xuwei-k", "qiita-twitter-bot"),
    Repo("xuwei-k", "play-json-extra"),
    Repo("xuwei-k", "httpz"),
    Repo("xuwei-k", "webpush-scala"),
    Repo("xuwei-k", "discourse-bot"),
    Repo("xuwei-k", "httpmock"),
    // Repo("xuwei-k", "msgpack-json"),
    // Repo("xuwei-k", "javadoc-badge"),
    Repo("xuwei-k", "scalaz-magnolia"),
    Repo("xuwei-k", "iarray"),
    Repo("xuwei-k", "githubot"),
    Repo("xuwei-k", "play2scalaz"),
    Repo("xuwei-k", "mima"),
    Repo("xuwei-k", "sonatype"),
    Repo("xuwei-k", "optparse-applicative"),
    Repo("xuwei-k", "sbt-proguard"),
    Repo("xuwei-k", "pj"),
    // Repo("xuwei-k", "sbt-jshell"), TODO jdk 9+
    Repo("xuwei-k", "zeroapply"),
    Repo("xuwei-k", "sbt-class-diagram"),
    Repo("xuwei-k", "nobox"),
    Repo("xuwei-k", "sbt-conflict-classes"),
    Repo("xuwei-k", "scalajspack"),
    Repo("xuwei-k", "wartremover-scalikejdbc"),
    Repo("xuwei-k", "scodec-msgpack"),
    Repo("scalaprops", "scalaprops"),
    Repo("scalaprops", "scalaprops-deriving"),
    Repo("scalaprops", "scalaprops-native-example"),
    Repo("scalaprops", "scalaprops-examples"),
    Repo("scalaprops", "scalaprops-cross-example"),
    Repo("scalaprops", "scalaprops-magnolia"),
    Repo("scalaprops", "scalaprops-shapeless"),
    Repo(
      "scalaprops",
      "sbt-scalaprops",
      testCommands = List(
        "test"
      )
    ),
    Repo("msgpack4z", "msgpack4z-core"),
    Repo("msgpack4z", "msgpack4z-java"),
    Repo("msgpack4z", "msgpack4z-native"),
    Repo("msgpack4z", "msgpack4z-circe"),
    Repo("msgpack4z", "msgpack4z-argonaut"),
    Repo("msgpack4z", "msgpack4z-play"),
    Repo("msgpack4z", "msgpack4z-jawn"),
    Repo("msgpack4z", "msgpack4z-api"),
    Repo("msgpack4z", "msgpack4z-native"),
    Repo("scalapb-json", "scalapb-playjson"),
    Repo("scalapb-json", "scalapb-circe"),
    Repo("scalapb-json", "scalapb-argonaut"),
    Repo("scalapb-json", "scalapb-json-common"),
    Repo("scalapb-json", "protoc-lint")
  ).distinct.map(_.copy(createPullRequest = true))

  val anotherRepos = List(
    Repo("sirthias", "parboiled2"),
    Repo("sirthias", "parboiled", filter = u => {
      u.groupId =!= "org.testng"
    }),
    Repo("foundweekends", "conscript"),
    Repo("foundweekends", "pamflet"),
    Repo("squeryl", "squeryl", filter = u => {
      u.groupId =!= "org.apache.derby"
    }),
    Repo("gitbucket", "gitbucket", filter = u => {
      !Set(
        "com.h2database",
        "com.nimbusds",
        "com.novell.ldap",
        "org.apache.sshd",
        "org.ec4j.core",
        "com.typesafe.akka", // don't update akka 2.6
        "com.enragedginger" //  don't update akka 2.6
      ).contains(u.groupId)
    }),
    Repo("nscala-time", "nscala-time"),
    Repo("scala-text", "scala_text"),
    // Repo("seratch", "AWScala"),
    // Repo("scopt", "scopt"),
    // Repo("scalaj", "scalaj-http"),
    // Repo("flyway", "flyway-play"),
    Repo(
      "scalikejdbc",
      "scalikejdbc",
      filter = u => {
        // tests fail with h2 version 1.4.200
        (u.groupId =!= "org.apache.derby") &&
        (u.groupId =!= "com.h2database") &&
        (u.groupId =!= "ch.epfl.lamp") && // TODO re-enable when dotty 3.0.0-M1 released
        (u.groupId =!= "org.hsqldb")
      }
    ),
    //   Repo("skinny-framework", "skinny-micro"),
    //   Repo("skinny-framework", "skinny-framework"),
    /*
    Repo("scalate", "scalate", filter = u => {
     u.groupId =!= "org.atmosphere"
    }),
    Repo("scalatra", "scalatra", filter = u => {
      u.groupId =!= "io.dropwizard.metrics" &&
      u.groupId =!= "nl.grons" &&
      u.groupId =!= "org.scalatra.scalate" &&
      u.groupId =!= "org.apache.httpcomponents" // tests fail
    }),
    Repo("scalatra", "scalamd"),
    Repo(
      "scalatra",
      "sbt-scalatra",
      testCommands = List(
        "^scripted"
      )
    ),
    Repo("scalatra", "scalatra.g8"),
    */
    Repo(
      "sbt",
      "sbt-protobuf",
      testCommands = List(
        "test",
        "^scripted"
      )
    ),
    Repo("sbt", "sbt-appengine"),
    Repo(
      "sbt",
      "sbt-assembly",
      testCommands = List(
        "test",
        // "^scripted" // too slow
      )
    ),
    /*
    Repo(
      "sbt",
      "sbt-buildinfo",
      testCommands = List(
        "test",
        "^scripted"
      )
    ),
    */
    Repo(
      "sbt",
      "sbt-ghpages",
      testCommands = List(
        "test",
        "^scripted"
      )
    ),
    /*
    Repo(
      "sbt",
      "sbt-sriracha",
      testCommands = List(
        "test",
        "^scripted"
      )
    ),
    Repo("sbt", "contraband"),
    */
    // Repo("sbt", "util"),
    // Repo("sbt", "io"),
    // TODO: not found sbt-git 0.8.5 (sbt-houserules 0.3.1 depends on sbt-git)
    // https://github.com/sbt/sbt-houserules/blob/v0.3.1/build.sbt#L13
    // https://dl.bintray.com/sbt/sbt-plugin-releases/com.typesafe.sbt/sbt-git/scala_2.10/sbt_0.13/
    /*
    Repo(
      "sbt",
      "launcher",
      testCommands = List(
        "compile" // https://github.com/sbt/launcher/blob/fbe82523cdfe5453642f1dccbd2415cc806093a1/project/Release.scala#L55
      )
    ),
    */
    // Repo("sbt", "zinc"),
    // Repo("sbt", "sbinary"),
    // Repo("sbt", "junit-interface"),
    Repo(
      "tototoshi",
      "sbt-slick-codegen",
      testCommands = List(
        "test",
        "^scripted"
      )
    ),
    // Repo("tototoshi", "slick-joda-mapper"),
    Repo("tototoshi", "scala-csv"),
    Repo("folone", "poi.scala"),
    // Repo("xdotai", "play-json-extensions"),
    // Repo("eed3si9n", "gigahorse"),
    // Repo("eed3si9n", "sjson-new"),
    // Repo("btlines", "grpcgateway"),
    // Repo("btlines", "pbdirect"),
    // Repo("btlines", "grpcmonix"),
    // Repo("btlines", "grpcakkastream"),
    Repo("b-studios", "scala-effekt", filter = u => {
      u.groupId =!= "com.47deg" // TODO sbt-microsites
    }),
    Repo("runarorama", "latr"),
    // Repo("debasishg", "scala-redis"),
    // Repo("t2v", "holidays"),
    // Repo("dispatch", "reboot"),
    Repo("etaty", "rediscala"),
    // Repo("scalaz", "scalaz-deriving"), TODO update scalafix dependency
  ).distinct.map(_.copy(createPullRequest = false))

  val repositories = (defaultRepos ::: anotherRepos).sortBy(_.show)

  def partial[A](x: Int, y: Int, values: List[A]): List[A] = {
    val n0 = math.max(values.size / y, 0)
    val n = if (values.size % y === 0) n0 else n0 + 1
    if (x >= y) {
      values.drop(x * n)
    } else {
      values.drop(x * n).take(n)
    }
  }

  def getRepos(args: List[String]): List[Repo] =
    args match {
      case Nil =>
        Random.shuffle(repositories)
      case Seq(RunPartial((x, y))) =>
        partial(x, y, repositories)
      case repos =>
        repos.map { name =>
          val Array(user, repo) = name.split('/')
          Repo(user, repo)
        }
    }

}
