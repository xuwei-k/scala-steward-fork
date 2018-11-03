package eu.timepit.scalasteward

import org.scalacheck._

object StewardProps extends Properties("steward") {
  def g[X: Arbitrary]: Gen[(Int, List[X])] =
    for {
      values <- Gen.nonEmptyListOf(implicitly[Arbitrary[X]].arbitrary)
      x <- Gen.chooseNum(1, values.size)
    } yield (x, values)

  property("partial") = Prop.forAllNoShrink(g[Int]) {
    case (y, values) =>
      val z = (0 until y).map { x =>
        steward.partial(x, y, values)
      }
      z.flatten == values
  }
}
