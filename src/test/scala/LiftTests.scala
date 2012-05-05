import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class LiftTests extends FunSuite with ShouldMatchers {
  import EitherExtras._
  test("Int literal succeeds, left-hand type is Any") {
    1.succeed should equal(Right[Any, Int](1))
  }
  test("String ref succeeds, left-hand type is Any") {
    val value = "something"
    value.succeed should equal(Right[Any, String](value))
  }
  test("String literal fails, right-hand type is Any") {
    "no good".fail should equal(Left[String, Any]("no good"))
  }
  test("Int ref fails, left-hand type is Int, right-hand type is Any") {
    val value = -1
    value.fail should equal(Left[Int, Any](value))
  }

  type T = (Boolean, Int)
  type E = Either[String, T]
  def checkTrue(t: T): E = if (t._1 == true) Right(t) else Left("Boolean field was "+ t._1)
  def checkPositive(t: T): E = if (t._2 > 0) Right(t) else Left("Int field not > 0: "+ t._2)
  val badValue: T = (false, -1)
  val goodValue: T = (true, 1)
  object expected {
    object fast {
      val succ = Right[String, T](goodValue)
      val fail = Left[String, T]("Boolean field was "+ badValue._1)
    }
    object slow {
      val succ = Right[List[String], T](goodValue)
      val fail = {
        val msgs = List("Boolean field was "+ badValue._1, "Int field not > 0: "+ badValue._2)
        Left[List[String], T](msgs)
      }
    }
  }

  test("fastCheck (without mapping) bad value") {
    badValue.fastCheck(checkTrue, checkPositive) should equal(expected.fast.fail)
  }
  test("slowCheck (without mapping) bad value") {
    badValue.slowCheck(checkTrue, checkPositive) should equal(expected.slow.fail)
  }
  test("fastCheck (without mapping) good value") {
    goodValue.fastCheck(checkTrue, checkPositive) should equal(expected.fast.succ)
  }
  test("slowCheck (without mapping) good value") {
    goodValue.slowCheck(checkTrue, checkPositive) should equal(expected.slow.succ)
  }
  test("slowCheckAndMap bad value") {
    def f(t: T) = t.toString
    badValue.slowCheckAndMap(checkTrue, checkPositive)(f) should equal(expected.slow.fail)
  }
  test("fastCheckAndMap bad value") {
    def f(t: T) = t.toString
    badValue.fastCheckAndMap(checkTrue, checkPositive)(f) should equal(expected.fast.fail)
  }
  test("slowCheckAndMap good value") {
    val expectedValue = Right[List[String], String](goodValue.toString)
    def f(t: T) = t.toString
    goodValue.slowCheckAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  }
  test("fastCheckAndMap good value") {
    val expectedValue = Right[List[String], String](goodValue.toString)
    def f(t: T) = t.toString
    goodValue.fastCheckAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  }
  test("slowCheck taking Seq, bad value") {
    val checks = Seq(checkTrue _, checkPositive _)
    badValue.slowCheck(checks: _*) should equal(expected.slow.fail)
  }
  test("fastCheck taking Seq, bad value") {
    val checks = Seq(checkTrue _, checkPositive _)
    badValue.fastCheck(checks: _*) should equal(expected.fast.fail)
  }
}

