class EitherOps[L] {
  trait Enhancer1[R1, R2] {
    /**
     * confers applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts. */
    def <*>(either: Either[L, R1]): Either[L, R2]
  }
  trait Enhancer2[R1, R2] {
    /**
     * as for <*> but accumulates the left-hand results. */
    def <**>(either: Either[L, R1]): Either[List[L], R2]
  }
  /**
   * lifts f, which must be curried, into a Right. */
  def lift1[R1, R2](f: R1 => R2) = Right[L,       R1 => R2](f)
  def lift2[R1, R2](f: R1 => R2) = Right[List[L], R1 => R2](f)

  implicit def enhance1[R1, R2](f: Either[L, R1 => R2]): Enhancer1[R1, R2] =
    new Enhancer1[R1, R2] {
      def <*>(either: Either[L, R1]) = (f, either) match {
        case (Left(l), _) => Left(l)
        case (Right(_), Left(l)) => Left(l)
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
  implicit def enhance2[R1, R2](f: Either[List[L], R1 => R2]): Enhancer2[R1, R2] =
    new Enhancer2[R1, R2] {
      def <**>(either: Either[L, R1]) = (f, either) match {
        case (Left(ls), Left(l)) => Left(l::ls)
        case (Left(ls), Right(r)) => Left(ls)
        case (Right(_), Left(l)) => Left(List(l))
        case (Right(f), Right(r1)) => Right(f(r1))
      }
    }
}
