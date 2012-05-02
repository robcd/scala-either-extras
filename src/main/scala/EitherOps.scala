trait EitherOps[L, R1, R2] {
  /**
   * confers applicative-functor status: lets you apply a FunctionN to N values in N contexts. */
  def <*>(either: Either[L, R1]): Either[L, R2]
}

object EitherOps {
  implicit def eitherOfFunction2EitherOps[L, R1, R2](f: Either[L, R1 => R2]): EitherOps[L, R1, R2] =
    new EitherOps[L, R1, R2] {
      def <*>(either: Either[L, R1]) = (f, either) match {
        case (Left(l), _) => Left(l)
        case (Right(_), Left(l)) => Left(l)
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
}
