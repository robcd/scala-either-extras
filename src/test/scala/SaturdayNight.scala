/*
 * <p>This code is derived from a Github Gist written by Chris Marshall:
 *   https://gist.github.com/970717
 * </p>
 * <p>It is essentially a copy of a fork of the above, made by Rob Dickens:
 *   https://gist.github.com/1241855
 * </p>
 */
import org.lafros.scala.EitherExtras
/**
 * Part Zero : 10:15 Saturday Night
 */
object Sobriety extends Enumeration {
  val Sober, Tipsy, Drunk, Paralytic, Unconscious = Value
}

object Gender extends Enumeration {
  val Male, Female = Value
}

case class Person(
  gender: Gender.Value,
  age: Int,
  clothes: Set[String],
  sobriety: Sobriety.Value)

object people {
  val Ken = Person(Gender.Male, 28, Set("Tie", "Shirt"), Sobriety.Tipsy)
  val Dave = Person(Gender.Male, 41, Set("Tie", "Jeans"), Sobriety.Sober)
  val Ruby = Person(Gender.Female, 25, Set("High Heels"), Sobriety.Tipsy)
}
/**
 * Let's define a trait which will contain the checks that *all* nightclubs make! */
trait Nightclub {
  import EitherExtras._
  //First CHECK
  def checkAge(p: Person): Either[String, Person] =
    if (p.age < 18) "Too Young!".fail
    else if (p.age > 40) "Too Old!".fail
    else p.succeed

  //Second CHECK
  def checkClothes(p: Person): Either[String, Person] =
    if (p.gender == Gender.Male && !p.clothes("Tie"))
      "Smarten Up!".fail
    else if (p.gender == Gender.Female && p.clothes("Trainers"))
      "Wear high heels".fail
    else
      p.succeed

  //Third CHECK
  def checkSobriety(p: Person): Either[String, Person] =
    if (Set(Sobriety.Drunk, Sobriety.Paralytic, Sobriety.Unconscious) contains p.sobriety)
      "Sober Up!".fail
    else
      p.succeed
}
/**
 * Part One : Clubbed to Death
 *
 * Now let's compose some validation checks */
object ClubbedToDeath extends Nightclub {
  def costToEnter(p: Person): Either[String, Double] = {

    //PERFORM THE CHECKS USING Monadic "for comprehension" SUGAR
    for {
      a <- checkAge(p).right
      b <- checkClothes(a).right
      c <- checkSobriety(b).right
    } yield (if (c.gender == Gender.Female) 0D else 5D)
  }
}

trait Test {
  def run(block: => Any) {
    println(block)
  }
}

// Now let's see these in action
object Test1 extends App with Test {
  import people._
  // Let's go clubbing!
  run(ClubbedToDeath costToEnter Dave) //res0: scalaz.Validation[String,Double] = Failure(Too Old!)

  run(ClubbedToDeath costToEnter Ken) //res1: scalaz.Validation[String,Double] = Success(5.0)

  run(ClubbedToDeath costToEnter Ruby) //res2: scalaz.Validation[String,Double] = Success(0.0)

  run(ClubbedToDeath costToEnter (Ruby.copy(age = 17)))
  //res3: scalaz.Validation[String,Double] = Failure(Too Young!)

  run(ClubbedToDeath costToEnter (Ken.copy(sobriety = Sobriety.Unconscious)))
  //res5: scalaz.Validation[String,Double] = Failure(Sober Up!)
}
/**
 * The thing to note here is how the Validations can be composed together in a
 * for-comprehension.  * Scala's type system is making sure that failures flow through your
 * computation in a safe manner.
 */

/**
 * Part Two : Club Tropicana
 *
 * Part One showed monadic composition, which from the perspective of Validation is *fail-fast*.
 * That is, any failed check shortcircuits subsequent checks. This nicely models nightclubs in
 * the real world, as anyone who has dashed home for a pair of smart shoes and returned, only to
 * be told that your tie does not pass muster, will attest.
 *
 * But what about an ideal nightclub? One that tells you *everything* that is wrong with you.
 *
 */

object ClubTropicana extends Nightclub {
  def costToEnter(p: Person): Either[List[String], Double] = {
    import EitherExtras._
    p.slowCheckAndMap(checkAge, checkClothes, checkSobriety) { p =>
      if (p.gender == Gender.Female) 0D else 7.5D
    }
  }
}

/**
 *
 * And the use? Dave tried the second nightclub after a few more drinks in the pub
 *
 */
object Test2 extends App with Test {
  import people._

  run(ClubTropicana costToEnter (Dave.copy(sobriety = Sobriety.Paralytic)))
  //res6: scalaz.Scalaz.ValidationNEL[String,Double] = Failure(NonEmptyList(Too Old!, Sober Up!))

  run(ClubTropicana costToEnter(Ruby))
  //res7: scalaz.Scalaz.ValidationNEL[String,Double] = Success(0.0)
}
/**
 * So, what have we done? Well, with a *tiny change* (and no changes to the individual checks
 * themselves), we have completely changed the behaviour to accumulate all errors, rather than
 * halting at the first sign of trouble. Imagine trying to do this in Java, using exceptions,
 * with ten checks.
 */

/**
 * Part Three : Gay Bar
 *
 * And for those wondering how to do this with a *very long list* of checks. Use sequence:
 *   List[ValidationNEL[E, A]] ~> (via sequence) ~> ValidationNEL[E, List[A]]
 *
 * Here we go (unfortunately we need to use a type lambda on the call to sequence):
 */
object GayBar extends Nightclub {
  import EitherExtras._
  def checkGender(p: Person): Either[String, Person] =
    if (p.gender != Gender.Male) "Men Only".fail
    else p.succeed

  def costToEnter(p: Person): Either[List[String], Double] = {
    val checks = List(checkAge _, checkClothes _, checkSobriety _, checkGender _)
    p.slowCheckAndMap(checks: _*)(_.age + 1.5D)
  }
}

object Test3 extends App with Test {
  import GayBar._

  run(costToEnter(Person(Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic)))
  //Failure(NonEmptyList(Too Old!, Smarten Up!, Sober Up!))
}

/**
 * As always; the point is that our validation functions are "static";
 * we do not need to change the way they have been coded because we want to combine them in different ways
 */
