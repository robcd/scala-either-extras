object usingEitherApp2 extends App {
  trait UsingExceptions {
    type T
    def f(a: T, b: T): T
    def a: T
    def b: T

    val res = f(a, b)
  }

  trait UsingEither {
    this: EitherExtras =>
    type T
    def f(a: T)(b: T): T
    def a: T
    def b: T
    type E = Either[L, T]
    def check1(a: T): E
    def check2(a: T): E

    val res1 = fast(f) <*> a.fastCheck(check1, check2) <*>
                           b.fastCheck(check1, check2)

    val res2 = slow(f) <*> a.fastCheck(check1, check2) <*>
                           b.fastCheck(check1, check2)

    val res3 = slow(f) <*> a.slowCheck(check1, check2) <*>
                           b.slowCheck(check1, check2)

    val res4 = slow(f) <*> a.fastCheck(check1, check2) <*>
                           b.slowCheck(check1, check2)
  }

  trait Eg extends UsingEither with EitherExtras {
    type T = (Int, String)
    type L = String
    def f(a: T)(b: T) = (a._1 + b._1, a._2 +" & "+ b._2 +" items")
    def check1(a: T) = if (a._1 > 0) Right(a) else Left("must be at least one item")
    def check2(a: T) = a._2 match {
      case null => Left("name was null")
      case "" => Left("empty name")
      case _ => Right(a)
    }

    println(res1)
    println(res2)
    println(res3)
    println(res4)
  }
  new Eg {
    def a = (0, "")
    def b = (-1, null)

    // Left(must be at least one item)
    // Left(List(must be at least one item,
    //           must be at least one item))
    // Left(List(must be at least one item,
    //           empty name,
    //           must be at least one item,
    //           name was null))
    // Left(List(must be at least one item,
    //           must be at least one item,
    //           name was null))
  }
  new Eg {
    def a = (2, null)
    def b = (0, "tenor")

    // Left(name was null)
    // Left(List(name was null,
    //           must be at least one item))
    // Left(List(name was null,
    //           must be at least one item))
    // Left(List(name was null,
    //           must be at least one item))
  }
  new Eg {
    def a = (2, "alto")
    def b = (3, "")

    // Left(empty name)
    // Left(List(empty name))
    // Left(List(empty name))
    // Left(List(empty name))
  }
  new Eg {
    def a = (2, "alto")
    def b = (3, "tenor")

    // Right((5,alto & tenor items))
    // Right((5,alto & tenor items))
    // Right((5,alto & tenor items))
    // Right((5,alto & tenor items))
  }
}
