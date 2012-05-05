trait UsingExceptions[T] {
  def a: T
  def b: T
  def f(a: T, b: T): T

  f(a, b)
}

trait UsingEither[L, T] {
  this: EitherExtras[L] =>
  type E[T] = Either[L, T]
  def a: E[T]
  def b: E[T]
  def f(a: T)(b: T): T

  fast(f) <*> a <*> b
  slow(f) <*> a <*> b
}
