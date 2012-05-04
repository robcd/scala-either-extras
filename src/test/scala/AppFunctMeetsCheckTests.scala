import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class AppFunctMeetsCheckTests extends FunSuite with ShouldMatchers {
  object input {
    val allGood = """
1 12.34 A
2 45.67 B
"""
    val someGood = """
? 45.67 B
1 ????? C
1 89.01 ?
"""
    val allBad = """
? ????? ?
? ????? ?
"""
  }
  // object output {
  //   val allGood = 
  // }

  case class Record(qty: Int)(price: Double)(name: String)

  type L = String

  def readQty(s: String): Either[L, Int] = try {
    val qty = s.toInt
    if (qty < 0) Left("bad quantity: "+ qty)
    else Right(qty)

  } catch {
    case ex: Exception => Left("failed to parse quantity: "+ ex.toString)
  }

  def readPrice(s: String): Either[L, Double] = try {
    val price = s.toDouble
    if (price < 0) Left("bad price: "+ price)
    else Right(price)
  } catch {
    case ex: Exception => Left("failed to parse price: "+ ex.toString)
  }

  def readName(s: String): Either[L, String] =
    if (s matches "[A-Z]") Right(s) else Left("bad name: "+ s)

  import EitherExtras._

  def stringToRecords(s: String) = {
    val source = scala.io.Source.fromString(s)
    val lines = source.getLines
    for (line <- lines) yield {
      val tokens = line.split("\\s+")
      if (tokens.length < 3)
        Left[List[L], Record](List("only "+ tokens.length +" token(s) on line"))
      else
        slow(Record.apply) <*> readQty(tokens(0)) <*> readPrice(tokens(1)) <*> readName(tokens(2))
    }
  }

  test("all good") {
    stringToRecords(input.allGood) foreach println
  }
  test("some good") {
    stringToRecords(input.someGood) foreach println
  }
  test("all bad") {
    stringToRecords(input.allBad) foreach println
  }

  // type E[R] = Either[String, R]
  // def checkPositive(n: Int): E[Int] = if (n > 0) Right(n) else Left("Int not > 0: "+ n)
  // def checkTrue(b: Boolean): E[Boolean] = if (b == true) Right(b) else Left("Boolean was "+ b)
  // def checkNonEmpty(s: String): E[String] = if (s != "") Right(s) else Left("Empty String")

  // case class CompoundValue(n: Int)(b: Boolean)(s: String)

  // object allGood {
  //   val n = 1; val b = true; val s = "non-empty"

  //   val expected = Right(CompoundValue(n)(b)(s))
  // }

  // object allBad {
  //   val n = -1; val b = false; val s = ""

  //   object expected {
  //     val failFast = Left[String, String]("Int not > 0: "+ -1)
  //     val accumulated = {
  //       val msgs = List("Int not > 0: "+ -1, "Boolean was false", "Empty String")
  //       Left[List[String], String](msgs)
  //     }
  //   }
  // }

  // test("") {
  //   slow(CaseClass.apply) <*> get1stField().check(checkTrue, checkPositive)
  // }

  // test("fail-fast, data all good") {
  //   import EitherExtras._
  //   import allGood._
  //   val res =
  //     fast(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
  //   res should equal(expected)
  // }
  // test("fail-fast, data all bad") {
  //   import EitherExtras._
  //   import allBad._
  //   val res =
  //     fast(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
  //   res should equal(expected.failFast)
  // }
  // test("fail-slow, all data good") {
  //   import EitherExtras._
  //   import allGood._
  //   val res =
  //     slow(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
  //   res should equal(expected)
  // }
  // test("fail-slow, all data bad") {
  //   import EitherExtras._
  //   import allBad._
  //   val res =
  //     slow(CompoundValue.apply) <*> checkPositive(n) <*> checkTrue(b) <*> checkNonEmpty(s)
  //   res should equal(expected.accumulated)
  // }
}
