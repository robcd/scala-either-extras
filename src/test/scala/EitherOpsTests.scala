import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class EitherOpsTests extends FunSuite with ShouldMatchers {
  type E[R] = Either[String, R]
  def checkPositive(n: Int): E[Int] = if (n > 0) Right(n) else Left("Int not > 0: "+ n)
  def checkTrue(b: Boolean): E[Boolean] = if (b == true) Right(b) else Left("Boolean was "+ b)
  def checkNonEmpty(s: String): E[String] = if (s != "") Right(s) else Left("Empty String")

  case class CompoundValue(n: Int)(b: Boolean)(s: String)

  object allGood {
    val n = 1; val b = true; val s = "non-empty"

    val expected = Right(CompoundValue(n)(b)(s))
  }

  object allBad {
    val n = -1; val b = false; val s = ""

    object expected {
      val failFast = Left[String, String]("Int not > 0: "+ -1)
      val accumulated = {
        val msgs = List("Int not > 0: "+ -1, "Boolean was false", "Empty String")
        Left[List[String], String](msgs)
      }
    }
  }

  test("fail-fast, data all good") {
    import EitherOps._
    import allGood._
    val res =
      lift1(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
    res should equal(expected)
  }
  test("fail-fast, data all bad") {
    import EitherOps._
    import allBad._
    val res =
      lift1(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
    res should equal(expected.failFast)
  }
  test("accumulating msgs, all data good") {
    import EitherOps._
    import allGood._
    val res =
      lift2(CompoundValue.apply) <**> checkPositive(n) <**> checkTrue(b) <**> checkNonEmpty(s)
    res should equal(expected)
  }
  test("accumulating msgs, all data bad") {
    import EitherOps._
    import allBad._
    val res =
      lift2(CompoundValue.apply) <**> checkPositive(n) <**> checkTrue(b) <**> checkNonEmpty(s)
    res should equal(expected.accumulated)
  }
}

