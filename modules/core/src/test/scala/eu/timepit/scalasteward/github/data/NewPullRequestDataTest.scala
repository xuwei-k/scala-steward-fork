package eu.timepit.scalasteward.github.data

import cats.data.NonEmptyList
import eu.timepit.scalasteward.git.{Branch, Sha1}
import eu.timepit.scalasteward.model.Update
import eu.timepit.scalasteward.nurture.UpdateData
import io.circe.syntax._
import org.scalatest.{FunSuite, Matchers}

class NewPullRequestDataTest extends FunSuite with Matchers {
  test("asJson") {
    val data = UpdateData(
      Repo("foo", "bar"),
      Update.Single("ch.qos.logback", "logback-classic", "1.2.0", NonEmptyList.of("1.2.3")),
      Branch("master"),
      Sha1(Sha1.HexString("d6b6791d2ea11df1d156fe70979ab8c3a5ba3433")),
      Branch("update/logback-classic-1.2.3")
    )
  }
}
