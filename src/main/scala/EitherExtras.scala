trait EitherExtras {
  trait Lift[T] {
    def succeed[L]: Right[L, T]
    def fail[R]: Left[T, R]
    /**
     * as in fail-fast: returns at the first failure. */
    def fastCheck[L](checks: ((T) => Either[L, T])*): Either[L, T]
    /**
     * as in fail-slow: all checks are carried out. */
    def slowCheck[L](checks: ((T) => Either[L, T])*): Either[List[L], T]
    /**
     * applies f if there were no failures. */
    def fastCheckAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[L, R]
    def slowCheckAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[List[L], R]
  }

  implicit def any2Lift[T](any: T): Lift[T] = new Lift[T] {
    def succeed[L] = Right[L, T](any)
    def fail[R] = Left[T, R](any)
    def fastCheck[L](checks: ((T) => Either[L, T])*): Either[L, T] =
      fastCheckAndMap[L, T](checks: _*) { t => t }
    def fastCheckAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[L, R] = {
      var msg: Option[Left[L, R]] = None
      checks.find { check =>
        check(any) match {
          case Left(x) =>
            msg = Some(Left(x))
          true
          case _ => false
        }
      }
      msg.getOrElse(Right(f(any)))
    }
    def slowCheck[L](checks: ((T) => Either[L, T])*): Either[List[L], T] =
      slowCheckAndMap[L, T](checks: _*) { t => t }
    def slowCheckAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[List[L], R] = {
      val msgs = for {
        check <- checks.toList // from WrappedArray
        msg <- check(any).left.toSeq
      } yield msg
      if (msgs.isEmpty) Right(f(any)) else Left(msgs)
    }
  }

  type L

  trait FastAppFunct[R1, R2] {
    /**
     * confers fail-fast applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts. */
    def <*>(either: Either[L, R1]): Either[L, R2]
    /**
     * as above, using just the head of the list in the case of a Left. */
    def <**>(either: Either[List[L], R1]): Either[L, R2]
  }
  trait SlowAppFunct[R1, R2] {
    /**
     * as above but fail-slow: continues in order to accumulate further left-hand results. */
    def <*>(either: Either[L, R1]): Either[List[L], R2]
    /**
     * as above, keeping all the elements in the case of a Left. */
    def <**>(either: Either[List[L], R1]): Either[List[L], R2]
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
      def <**>(either: Either[List[L], R1]) = either match {
        case Left(ls) => <*>(Left[L, R1](ls.head))
        case Right(r1) => <*>(Right[L, R1](r1))
      }
    }
  implicit def eitherFun2Slow[R1, R2](f: Either[List[L], R1 => R2]): SlowAppFunct[R1, R2] =
    new SlowAppFunct[R1, R2] {
      def <*>(either: Either[L, R1]) = (f, either) match {
        case (Left(ls), Left(l)) => Left(ls :+ l)
        case (Left(ls), Right(_)) => Left(ls)
        case (Right(_), Left(l)) => Left(List(l))
        case (Right(f), Right(r1)) => Right(f(r1))
      }
      def <**>(either: Either[List[L], R1]) = (f, either) match {
        case (Left(ls1), Left(ls2)) => Left(ls1 ++ ls2)
        case (Left(ls), Right(_)) => Left(ls)
        case (Right(_), Left(ls)) => Left(ls)
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
}

object EitherExtras extends EitherExtras {
  type L = String
}
