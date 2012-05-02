trait EitherOps[L, R1] {
  /**
   * confers applicative-functor status: lets you apply a FunctionN to N values in N contexts. */
  def <*>[R2](f: Either[L, R1 => R2]): Either[L, R2]
}

object EitherOps {
  implicit def either2EitherOps[L, R1](either: Either[L, R1]): EitherOps[L, R1] =
    new EitherOps[L, R1] {
      def <*>[R2](f: Either[L, R1 => R2]) = (either, f) match {
        case (Left(l), _) => Left(l)
        case (Right(_), Left(l)) => Left(l)
        case (Right(r1), Right(f)) => Right(f(r1))
      }
    }
}
