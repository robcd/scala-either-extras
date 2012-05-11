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

object usingEitherApp extends App {
  trait UsingExceptions {
    type T
    def f(a: T, b: T): T
    def a: T
    def b: T

    val res = f(a, b)
  }

  trait UsingEither {
    this: EitherExtras =>
    type T
    type E = Either[L, T]
    def f(a: T)(b: T): T
    def a: E
    def b: E

    val res1 = fast(f) <*> a <*> b
    val res2 = slow(f) <*> a <*> b
  }

  trait Eg extends UsingEither with EitherExtras {
    type T = Int
    type L = String
    def f(a: T)(b: T) = a*b

    println(res1)
    println(res2)
  }
  new Eg {
    def a = Left("couldn't obtain a")
    def b = Left("couldn't obtain b")

    // Left(couldn't obtain a)
    // Left(List(couldn't obtain a, couldn't obtain b))
  }
  new Eg {
    def a = Right(2)
    def b = Left("couldn't obtain b")

    // Left(couldn't obtain b)
    // Left(List(couldn't obtain b))
  }
  new Eg {
    def a = Right(2)
    def b = Right(3)

    // Right(6)
    // Right(6)
  }
}
