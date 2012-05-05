trait UsingExceptions {
  type T
  def a: T
  def b: T
  def f(a: T, b: T): T

  val res = f(a, b)
}

trait UsingEither {
  this: EitherExtras =>
  type T
  type E[T] = Either[L, T]
  def a: E[T]
  def b: E[T]
  def f(a: T)(b: T): T

  val res1 = fast(f) <*> a <*> b
  val res2 = slow(f) <*> a <*> b
}

object test extends UsingEither with EitherExtras with App {
  type L = Exception
  type T = Int
  def a = Left(new Exception("couldn't obtain a"))
  def b = Left(new Exception("couldn't obtain b"))
  def f(a: T)(b: T) = a*b

  println(res1)
  println(res2)
}
