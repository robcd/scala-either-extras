trait UsingExceptions {
  type T
  def f(a: T, b: T): T
  def g: T
  def h: T

  val res = f(g, h)
}

trait UsingEither {
  this: EitherExtras =>
  type T
  type E[T] = Either[L, T]
  def f(a: T)(b: T): T
  def g: E[T]
  def h: E[T]

  val res1 = fast(f) <*> g <*> h
  val res2 = slow(f) <*> g <*> h
}

object test extends UsingEither with EitherExtras with App {
  type L = Exception
  type T = Int

  def f(a: T)(b: T) = a*b
  def g = Left(new Exception("couldn't obtain g"))
  def h = Left(new Exception("couldn't obtain h"))

  println(res1)
  println(res2)
}
