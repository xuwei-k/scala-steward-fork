package eu.timepit.scalasteward.maven
import java.net.URL

import cats.{Id, Monad}

import scala.xml.XML

trait MavenAlg[F[_]] {

  def gitHubScmUrl(groupId: String, artifactId: String, version: String): F[Option[String]]

}
object Main {
  def main(args: Array[String]): Unit = {
    val x = MavenAlg.create[Id].gitHubScmUrl("org.typelevel", "cats-core_2.12", "1.4.0")
    println(x)
  }
}

object MavenAlg {

  def create[F[_]](implicit F: Monad[F]): MavenAlg[F] =
    new MavenAlg[F] {
      override def gitHubScmUrl(
          groupId: String,
          artifactId: String,
          version: String): F[Option[String]] = F.point{
        val g = groupId.replace('.', '/')
        val url = new URL(s"https://repo1.maven.org/maven2/${g}/${artifactId}/${version}/${artifactId}-${version}.pom")
        val scm = (XML.load(url) \ "scm" \ "url").text
        if(scm.contains("github.com")) Some(scm) else None
      }
    }




}
