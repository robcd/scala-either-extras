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

object usingEitherApp3 extends App {
  case class MyCaseClass(qty: Int, name: String)
  object MyCaseClass {
    type E[T] = Either[String, T]
    def checkQty(qty: Int): E[Int] =
      if (qty > 0) Right(qty) else Left("qty must be > 0")
    def checkName(name: String): E[String] = name match {
      case null => Left("name was null")
      case "" => Left("empty name")
      case _ => Right(name)
    }
  }
  trait UsingEither { // as opposed to Exceptions
    this: EitherExtras =>
    type L = String
    def qty: Int
    def name: String

    import MyCaseClass._
    val res1 = slow((apply _).curried) <*> checkQty(qty) <*> checkName(name)
  }
  trait Case extends UsingEither with EitherExtras {
    println(res1)
  }
  new Case {
    def qty = 0
    def name = null

    // Left(List(qty must be > 0, name was null))
  }
  new Case {
    def qty = 2
    def name = ""

    // Left(List(empty name))
  }
  new Case {
    def qty = -1
    def name = "mouthpiece"

    // Left(List(qty must be > 0))
  }
  new Case {
    def qty = 2
    def name = "reed"

    // Right(MyCaseClass(2))
    // N.B. Looks like case class toString hasn't anticipated currying
  }
}
