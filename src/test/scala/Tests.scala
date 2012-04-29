import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class Tests extends FunSuite with ShouldMatchers {
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

  test("check (without mapping)") {
    val value: T = (false, -1)
    val expectedValue = {
      val msgs = List("Boolean field was "+ value._1, "Int field not > 0: "+ value._2)
      Left[List[String], T](msgs)
    }
    value.check(checkTrue, checkPositive) should equal(expectedValue)
  }
  ignore("checkAndMap") {
  }
}

