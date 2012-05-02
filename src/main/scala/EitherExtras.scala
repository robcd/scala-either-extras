trait EitherExtras[T] {
  def succeed[L]: Right[L, T]
  def fail[R]: Left[T, R]
  def check[L](checks: ((T) => Either[L, T])*): Either[List[L], T]
  def checkAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[List[L], R]
}

object EitherExtras {
  implicit def any2EitherExtras[T](any: T): EitherExtras[T] = new EitherExtras[T] {
    def succeed[L] = Right[L, T](any)
    def fail[R] = Left[T, R](any)
    def check[L](checks: ((T) => Either[L, T])*): Either[List[L], T] =
      checkAndMap[L, T](checks: _*) { t => t }
    def checkAndMap[L, R](checks: ((T) => Either[L, T])*)(f: (T) => R): Either[List[L], R] = {
      val msgs = for {
        check <- checks.toList // from WrappedArray
        msg <- check(any).left.toSeq
      } yield msg
      if (msgs.isEmpty) Right(f(any)) else Left(msgs)
    }
  }
}
