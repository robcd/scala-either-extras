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
import org.lafros.scala.EitherExtras
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class AppFunctTests2 extends FunSuite with ShouldMatchers {
  trait UsingExceptions {
    type T
    def f(a: T, b: T): T
    def g: T
    def h: T

    val res = f(g, h)
  }

  trait UsingEither {
    this: EitherExtras =>
      type T
    type E[T] = Either[L, T]
    def f(a: T)(b: T): T
    def g: E[T]
    def h: E[T]

    val res1 = fast(f) <*> g <*> h
    val res2 = slow(f) <*> g <*> h
  }

  trait Eg extends UsingEither with EitherExtras {
    type T = Int
    type L = String
    def f(a: T)(b: T) = a*b
  }

  val no_g = "couldn't obtain g"
  val no_h = "couldn't obtain h"

  test("g, h both Left") {
    new Eg {
      def g = Left(no_g)
      def h = Left(no_h)

      res1 should equal(Left(no_g))
      res2 should equal(Left(List(no_g, no_h)))
    }
  }
  test("g Right, h Left") {
    new Eg {
      def g = Right(2)
      def h = Left(no_h)

      res1 should equal(Left(no_h))
      res2 should equal(Left(List(no_h)))
    }
  }
  test("g, h both Right") {
    new Eg {
      def g = Right(2)
      def h = Right(3)

      res1 should equal(Right(6))
      res2 should equal(Right(6))
    }
  }
}
