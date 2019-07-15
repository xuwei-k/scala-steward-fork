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
    Repo("xuwei-k", "scalaz-docs"),
    Repo("xuwei-k", "scala-protobuf-docs"),
    Repo("xuwei-k", "scalikejdbc-flyway-sbt-example"),
    Repo("xuwei-k", "mima-web"),
    Repo("xuwei-k", "favorite_typo"),
    Repo("xuwei-k", "applybuilder"),
    Repo("xuwei-k", "qiitascala"),
    Repo("xuwei-k", "qiita-twitter-bot"),
    Repo("xuwei-k", "play-json-extra"),
    Repo("xuwei-k", "httpz"),
    Repo("xuwei-k", "webpush-scala"),
    Repo("xuwei-k", "discourse-bot"),
    Repo("xuwei-k", "httpmock"),
    Repo("xuwei-k", "msgpack-json"),
    Repo("xuwei-k", "javadoc-badge"),
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
    Repo("scalaprops", "scalaprops"),
    Repo("scalaprops", "scalaprops-native-example"),
    Repo("scalaprops", "scalaprops-examples"),
    Repo("scalaprops", "scalaprops-cross-example"),
    Repo("scalaprops", "scalaprops-magnolia"),
    Repo("scalaprops", "scalaprops-shapeless"),
    Repo("scalaprops", "sbt-scalaprops"),
    Repo("msgpack4z", "msgpack4z-core"),
    Repo("msgpack4z", "msgpack4z-java"),
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
  ).distinct.map(_.copy(createPullRequest = true))

  val anotherRepos = List(
    Repo("sirthias", "parboiled2"),
    Repo("sirthias", "parboiled"),
    Repo("foundweekends", "giter8"),
    Repo("foundweekends", "knockoff"),
    Repo("foundweekends", "conscript"),
    Repo("foundweekends", "pamflet"),
    Repo("argonaut-io", "argonaut"),
    Repo("squeryl", "squeryl", filter = u => {
      u.groupId =!= "org.apache.derby"
    }),
    Repo("gitbucket", "gitbucket"),
    Repo("nscala-time", "nscala-time"),
    Repo("dwango", "S99"),
    Repo("dwango", "scala_text"),
    Repo("dwango", "slack-webhook-appender"),
    Repo("unfiltered", "unfiltered"),
    Repo("unfiltered", "website"),
    Repo("unfiltered", "unfiltered-websockets.g8"),
    Repo("unfiltered", "unfiltered-netty.g8"),
    Repo("unfiltered", "unfiltered-gae.g8"),
    Repo("unfiltered", "unfiltered.g8"),
    Repo("unfiltered", "unfiltered-scalate.g8"),
    Repo("unfiltered", "coffee-filter.g8"),
    Repo("unfiltered", "unfiltered-war.g8"),
    Repo("unfiltered", "unfiltered-slick.g8"),
    Repo("json4s", "json4s"),
    Repo("wartremover", "wartremover"),
    Repo("wartremover", "wartremover-contrib"),
    Repo("wartremover", "own-wart-example"),
    Repo("scopt", "scopt"),
    Repo("scalaj", "scalaj-http"),
    Repo("flyway", "flyway-play"),
    Repo("scalikejdbc", "scalikejdbc", filter = u => {
      (u.groupId =!= "org.mockito") && (u.groupId =!= "org.apache.derby")
    }),
    Repo("scalikejdbc", "scalikejdbc-play-support"),
    Repo("scalikejdbc", "scalikejdbc-async"),
    Repo("scalikejdbc", "csvquery", filter = u => {
      u.groupId =!= "com.h2database"
    }),
    //   Repo("skinny-framework", "skinny-micro"),
    Repo("skinny-framework", "skinny-framework"),
    Repo("scalate", "scalate"),
    Repo("scalatra", "scalatra"),
    Repo("scalatra", "scalamd"),
    Repo("scalatra", "sbt-scalatra"),
    Repo("scalatra", "scalatra.g8"),
    Repo("sbt", "sbt-protobuf"),
    Repo("sbt", "sbt-appengine"),
    Repo("sbt", "sbt-assembly"),
    Repo("sbt", "sbt-buildinfo"),
    Repo("sbt", "sbt-ghpages"),
    Repo("tototoshi", "sbt-slick-codegen"),
    Repo("tototoshi", "slick-joda-mapper"),
    Repo("tototoshi", "scala-csv"),
    Repo("folone", "poi.scala"),
    Repo("xdotai", "play-json-extensions"),
    Repo("iheartradio", "ficus"),
    Repo("eed3si9n", "expecty"),
    Repo("eed3si9n", "gigahorse"),
    Repo("eed3si9n", "sjson-new"),
    Repo("btlines", "grpcgateway"),
    Repo("btlines", "pbdirect"),
    Repo("btlines", "grpcmonix"),
    Repo("btlines", "grpcakkastream"),
    Repo("etorreborre", "specs2"),
    Repo("atnos-org", "eff"),
    Repo("atnos-org", "origami"),
    Repo("atnos-org", "producer"),
    Repo("runarorama", "latr"),
    Repo("t2v", "holidays"),
    Repo("etaty", "rediscala"),
    //    Repo("scalaz", "scalaz-deriving"), // TODO
    Repo("scalaz", "testz"),
    Repo("scalaz", "scalaz-sbt"),
    Repo("scalapb", "scalapb-grpcweb"),
    Repo("scalapb", "scalapb-template.g8")
  ).distinct.map(_.copy(createPullRequest = false))

  private[this] val lasts = List(
    Repo("scalaz", "scalaz")
  )

  val repositories = (defaultRepos ::: anotherRepos).sortBy(_.show) ::: lasts

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
