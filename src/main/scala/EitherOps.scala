class EitherOps[L] {
  trait FastAppFunct[R1, R2] {
    /**
     * confers fail-fast applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts. */
    def <*>(either: Either[L, R1]): Either[L, R2]
  }
  trait SlowAppFunct[R1, R2] {
    /**
     * as above but fail-slow: continues in order to accumulate further left-hand results. */
    def <*>(either: Either[L, R1]): Either[List[L], R2]
  }
  /**
   * lifts f, which must be curried, into a Right, for fail-fast application. */
  def fast[R1, R2](f: R1 => R2) = Right[L,       R1 => R2](f)
  /**
   * as above, but for fail-slow application. */
  def slow[R1, R2](f: R1 => R2) = Right[List[L], R1 => R2](f)

  implicit def eitherFun2Fast[R1, R2](f: Either[L, R1 => R2]): FastAppFunct[R1, R2] =
    new FastAppFunct[R1, R2] {
      def <*>(either: Either[L, R1]) = (f, either) match {
        case (Left(l), _) => Left(l)
        case (Right(_), Left(l)) => Left(l)
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
  implicit def eitherFun2Slow[R1, R2](f: Either[List[L], R1 => R2]): SlowAppFunct[R1, R2] =
    new SlowAppFunct[R1, R2] {
      def <*>(either: Either[L, R1]) = (f, either) match {
        case (Left(ls), Left(l)) => Left(ls :+ l)
        case (Left(ls), Right(r)) => Left(ls)
        case (Right(_), Left(l)) => Left(List(l))
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
}

object EitherOps extends EitherOps[String]
