/*
 * Copyright 2012 Latterfrosken Software Development Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.lafros.scala
/**
 * @author Rob Dickens
 * Contributors: Peter Robinett
 */
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
     * handles a fail-slow result, where Left contains a List. */
    def <*>[M](either: Either[M, R1])(implicit ev: M <:< List[L]): Either[L, R2]
    /**
     * confers fail-fast applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts, failing as soon as the first Left is encountered. */
    def <*>(either: Either[L, R1]): Either[L, R2]
  }
  trait SlowAppFunct[R1, R2] {
    /**
     * handles a fail-slow result, where Left contains a List. */
    def <*>[M](either: Either[M, R1])(implicit ev: M <:< List[L]): Either[List[L], R2]
    /**
     * confers fail-slow applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts, continuing to accumulate further left-hand results after the first is
     * encountered. */
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
      def <*>[M](either: Either[M, R1])(implicit ev: M <:< List[L]) = either match {
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
      def <*>[M](either: Either[M, R1])(implicit ev: M <:< List[L]) = (f, either) match {
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
