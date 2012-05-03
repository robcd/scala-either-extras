class EitherOps[L] {
  trait Enhancer[R1, R2] {
    /**
     * confers applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts. */
    def <*>(either: Either[L, R1]): Either[L, R2]
  }

  def lift[R1, R2](f: R1 => R2) = Right[L, R1 => R2](f)

  implicit def enhance[R1, R2](f: Either[L, R1 => R2]): Enhancer[R1, R2] = new Enhancer[R1, R2] {
    def <*>(either: Either[L, R1]) = (f, either) match {
      case (Left(l), _) => Left(l)
      case (Right(_), Left(l)) => Left(l)
      case (Right(f), Right(r1)) => Right(f(r1))
    }
  }
}
