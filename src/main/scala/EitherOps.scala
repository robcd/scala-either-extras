/**
 * lets you apply a FunctionN to N values in N contexts */
trait ApplicativeFunctor[A, M[A]] extends Functor[A, M] {
  def <*>[B](mf: M[A => B]): M[B]
}

class EitherApplicativeFunctor[A](eitherA: Either[_, A])
extends EitherFunctor(eitherA) with ApplicativeFunctor[A, ({type λ[A] = Either[_, A]})#λ] {
  def <*>[B](eitherA2B: Either[Any, A => B]) = (eitherA, eitherA2B) match {
    case (Left(x), _) => Left(x) // since, here, x is not of type A, and therefore may not be
    // passed to f
    case (Right(_), Left(x)) => Left(x)
    case (Right(a), Right(f)) => Right(f(a))
  }
}
