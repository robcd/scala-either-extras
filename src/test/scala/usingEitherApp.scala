object usingEitherApp extends App {
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
    type E = Either[L, T]
    def f(a: T)(b: T): T
    def g: E
    def h: E

    val res1 = fast(f) <*> g <*> h
    val res2 = slow(f) <*> g <*> h
  }

  trait Eg extends UsingEither with EitherExtras {
    type T = Int
    type L = String
    def f(a: T)(b: T) = a*b

    println(res1)
    println(res2)
  }
  new Eg {
    def g = Left("couldn't obtain g")
    def h = Left("couldn't obtain h")

    // Left(couldn't obtain g)
    // Left(List(couldn't obtain g, couldn't obtain h))
  }
  new Eg {
    def g = Right(2)
    def h = Left("couldn't obtain h")

    // Left(couldn't obtain h)
    // Left(List(couldn't obtain h))
  }
  new Eg {
    def g = Right(2)
    def h = Right(3)

    // Right(6)
    // Right(6)
  }
}
