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

  test("check (without mapping) bad value") {
    val value: T = (false, -1)
    val expectedValue = {
      val msgs = List("Boolean field was "+ value._1, "Int field not > 0: "+ value._2)
      Left[List[String], T](msgs)
    }
    value.check(checkTrue, checkPositive) should equal(expectedValue)
  }
  test("check (without mapping) good value") {
    val value: T = (true, 1)
    val expectedValue = Right[List[String], T](value)
    value.check(checkTrue, checkPositive) should equal(expectedValue)
  }
  test("checkAndMap bad value") {
    val value: T = (false, -1)
    val expectedValue = {
      val msgs = List("Boolean field was "+ value._1, "Int field not > 0: "+ value._2)
      Left[List[String], T](msgs)
    }
    def f(t: T) = t.toString
    value.checkAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  }
  test("checkAndMap good value") {
    val value: T = (true, 1)
    val expectedValue = Right[List[String], String](value.toString)
    def f(t: T) = t.toString
    value.checkAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  }
  test("check taking Seq") {
    val value: T = (false, -1)
    val checks = Seq(checkTrue _, checkPositive _)
    val expectedValue = {
      val msgs = List("Boolean field was "+ value._1, "Int field not > 0: "+ value._2)
      Left[List[String], T](msgs)
    }
    value.check(checks: _*) should equal(expectedValue)
  }
}

