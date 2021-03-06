package scalazPlayground

import scalaz._
import Scalaz._


case class Error(message: String, stackTrace: String)

// unify places where we have seqs of erros with where we only return one error:
object Error {
    implicit def ErrorSemigroup: Semigroup[Error] = semigroup((e1, e2) =>
        if (e1.message === ErrorZero.zero.message) {
            Error(e2.message, e2.stackTrace)
        } else {
            Error(e1.message + ", " + e2.message, e1.stackTrace + ", " + e2.stackTrace)
        }
    )

    implicit def ErrorZero: Zero[Error] = zero(Error("", ""))

    implicit def errorsToError(errors: Seq[Error]) = errors.asMA.sum
}

object ThePlayground {
    private def p(s: String) = println(s)

    private def assertEquals(expected: Any, actual: Any) = {
        if (expected == actual) {
            p("Pass: '%s' equals '%s'".format(expected, actual))
        }
        else {
            p("Fail: expect '%s' but got '%s'".format(expected, actual))
        }
    }

    def main(args: Array[String]) {
        p("** Welcome to the Scalaz playground **")

        import Error._

        assertEquals(some(1) |+| some(2), some(3))
        assertEquals(List(1) |+| List(2), List(1, 2))

        assertEquals(
            List(1, 2, 3) >>= { i =>
                if (i % 2 == 0) List(i) else Nil },
            List(2))

        assertEquals(
            for (i <- List(1, 2, 3)
                if (i % 2 == 0))
            yield i,
            List(2))

        assertEquals((List(1, 2, 3) |@| List(4)).apply { _ + _ }, List(5, 6, 7))

        assertEquals((List(1, 2, 3) <*> List((i: Int) => i + 4)),
                                                             List(5, 6, 7))
        assertEquals((List(1, 2, 3).map(_ + 4)), List(5, 6, 7))

        assertEquals(List(1, 2, 3).maximum, Some(3))

        assertEquals(List(1, 2, 3).any(_ > 2), true)
        assertEquals(List(1, 2, 3).any(_ > 4), false)

        assertEquals(List(55).getOrElseM(List(Some(77))), List(77))

        List(1, 2, 3) |>| { println(_) } // => 1, 2, 3

        (Failure("doh!"): Validation[String, Int]) match {
            case Success(i) =>   println(i + 5)
            case Failure(msg) => println(msg)
        }

        (Success(12): Validation[String, Int]) match {
            case Success(i) =>   println(i + 5)
            case Failure(msg) => println(msg)
        }

        (Success("yeah!"): Validation[String, String]) match {
            case Success(msg) => println(msg)
            case Failure(_)   => println("fail!")
        }

        // assertEquals(
        //     List(1, 2, 3) =>> { xs => xs.head },
        //     List(1))

        assertEquals("a" ?? "b", "a")
        assertEquals((null: String) ?? "b", "b")
        assertEquals(1 min 2, 1)

        assertEquals( 3 |> (_ + 4)
                        |> (_ * 9)
                        |> (_ * 10), 630)

        assertEquals( 1 ?|? 2, LT)

        assertEquals(false ?? List(1, 2, 3), List())
        assertEquals(false ?? Option(3), None)


        val either: Either[String, Int] = Right(55)
        assertEquals(either.flatMap(e => Right(59)), Right(59))

        assertEquals(List(1, 2, 3) >| "Hello World", List("Hello World",
                                                          "Hello World",
                                                          "Hello World"))

        assertEquals(Seq(some(1), some(2)).sequence, some(Seq(1, 2)))
        assertEquals(some(Seq(1, 2)).sequence, Seq(some(1), some(2)))

        assertEquals(Map("a" -> 1, "b" -> 4, "c" -> 3),
                     Map("a" -> 1, "b" -> 2) |+| Map("b" -> 2, "c" -> 3))

        assertEquals((3, "alex", List(4, 5)), (2, "al", List(4)) |+| (1, "ex", List(5)))

        assertEquals(Error("short, ribs", "stack, overflow"), Error("short", "stack") |+| Error("ribs", "overflow"))

        def needsOneError(e: Error) = e

        assertEquals(Error("a, 1", "b, 2"), needsOneError(Seq(Error("a", "b"), Error("1", "2"))))
    }
}
