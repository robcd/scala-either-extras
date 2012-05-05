object app extends App {
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

  trait Eg extends UsingEither with EitherExtras {
    type T = Int
    type L = Exception
    def f(a: T)(b: T) = a*b

    println(res1)
    println(res2)
  }
  new Eg {
    def g = Left(new Exception("couldn't obtain g"))
    def h = Left(new Exception("couldn't obtain h"))

    // Left(java.lang.Exception: couldn't obtain g)
    // Left(List(java.lang.Exception: couldn't obtain g, java.lang.Exception: couldn't obtain h))
  }
  new Eg {
    def g = Right(2)
    def h = Left(new Exception("couldn't obtain h"))

    // Left(java.lang.Exception: couldn't obtain h)
    // Left(List(java.lang.Exception: couldn't obtain h))
  }
  new Eg {
    def g = Right(2)
    def h = Right(3)

    // Right(6)
    // Right(6)
  }
}
