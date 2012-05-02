trait EitherOps[L, R1] {
  /**
   * confers applicative-functor status: lets you apply a FunctionN to N values in N contexts. */
  def <*>[R2](f: Either[L, R1 => R2]): Either[L, R2]
}

object EitherOps {
  implicit def either2EitherOps[L, R1](either: Either[L, R1]): EitherOps[L, R1] =
    new EitherOps[L, R1] {
    def <*>[R2](f: Either[L, R1 => R2]) = (either, f) match {
      case (Left(x), _) => Left(x) // since, here, x is not of type A, and therefore may not be
      // passed to f
      case (Right(_), Left(x)) => Left(x)
      case (Right(a), Right(f)) => Right(f(a))
    }
  }
}
