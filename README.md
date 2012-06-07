The original version of the type class in question is now to be found
in the [simple branch](scala-either-extras/tree/simple) of the project.

That version just provided some implicit methods for *lifting* a value
into a `scala.Either`. Of those, the `check` and `checkAndMap` methods
(for applying multiple checks to a single value) have now been
replaced by `fastCheck`, `fastCheckAndMap`, `slowCheck` and
`slowCheckAndMap`, to allow either *fail-fast* or *fail-slow*
application of the tests - in the event of a failure, the method
either returns immediately, with that single left-hand result, or it
completes all the checks, and returns all the left-hand results (in a
`List`).

The current version now also provides support that allows `Either` to be
used as an *applicative functor*.

This is useful when you want to pass validated values to a function
having multiple arguments, ensuring that the function is only called
when all the values are valid.

Such a function is first lifted into an `Either`, by passing it to the
provided `fast` or `slow` method (for either fail-fast or fail-slow
application).  This is then followed by the `<*>` operator (borrowed from
Haskell), which is in turn followed by checks corresponding to each
argument of the function, separated by further `<*>`s.

For example, `fast(f) <*> myCheck1(a) <*> myCheck2(b)`, where `f` has
two corresponding arguments.

Note that this may be combined with the implicit methods provided for
applying multiple checks to a single value, like so:

    slow(f) <*> a.slowCheck(myCheck1, myCheck3) <*> b.slowCheck(myCheck2, myCheck4)

The [new
EitherExtras](scala-either-extras/blob/master/src/main/scala/EitherExtras.scala)
trait is itself no longer a type class, but rather an outer trait,
having, now, three type classes as inner traits.

It may therefore now be brought into scope in different ways.

The method used originally, namely to add `import EitherExtras._`, is
still available, but this then sets the left-hand type as `String`, when
using `Either` as an applicative functor.

To set the left-hand type yourself, you can create your own singleton
object, and import that:

    object MyEitherExtras extends EitherExtras {
      type L = SomethingOtherThanString
      // N.B. If you forget to supply an abstract type, you won't
      // (nowadays) hear about it from the compiler until something
      // tries to use it, which can be problematic if that something
      // is compiled separately :-(
    }

Otherwise, you can mix it into your self type, or extend it straight
away.

For example apps, please see
[SaturdayNight](scala-either-extras/blob/master/src/test/scala/SaturdayNight.scala)
(cf. [original code](https://gist.github.com/1241855) written using
[Scalaz](http://code.google.com/p/scalaz/)),
[usingEitherApp](scala-either-extras/blob/master/src/test/scala/usingEitherApp.scala),
[usingEitherApp2](scala-either-extras/blob/master/src/test/scala/usingEitherApp2.scala)
and
[usingEitherApp3](scala-either-extras/blob/master/src/test/scala/usingEitherApp3.scala).

The tests class are
[LiftTests](scala-either-extras/blob/master/src/test/scala/LiftTests.scala),
[AppFunctTests](scala-either-extras/blob/master/src/test/scala/AppFunctTests.scala)
and
[AppFunctTests2](scala-either-extras/blob/master/src/test/scala/AppFunctTests2.scala).

Further Reading
---------------

[Validation without Scalaz](http://robsscala.blogspot.co.uk/2012/04/validation-without-scalaz.html)  
[Validating multiple values at once (without Scalaz)](http://robsscala.blogspot.co.uk/2012/05/validating-multiple-values-at-once.html)  
[EitherExtras class files now published](http://robsscala.blogspot.co.uk/2012/05/eitherextras-class-files-now-published.html)

Artifacts
---------

org.lafros artifacts in [Maven central repository](http://search.maven.org/#browse%7C238533119)  

org.lafros artifacts in [Sonatype mirror](http://oss.sonatype.org/content/groups/public/org/lafros)
