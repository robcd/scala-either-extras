The type class in question is called
[EitherExtras](/scala-either-extras/blob/simple/src/main/scala/EitherExtras.scala).

It simply provides a few methods for *lifting* a value into an `Either`.

As well as the basic `succeed` and `fail`, there is `check`, which
takes a variable number of args of type `R => Either`, and returns a
corresponding `Either[List[L], R]`, and also `checkAndMap`, which also
takes a mapping function.

The way to use these methods is shown in the [Tests](/scala-either-extras/blob/simple/src/test/scala/Tests.scala) class.

See also how the two test apps in
[SaturdayNight.scala](/scala-either-extras/blob/simple/src/test/scala/SaturdayNight.scala)
compare with the [original code](https://gist.github.com/1241855) which uses Scalaz.
