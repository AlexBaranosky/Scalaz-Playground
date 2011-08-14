package scalazPlayground

import scalaz._
import Scalaz._

object ThePlayground {   
    private def p(s: String) = println(s)
  
    private def assertEquals(expected: Any, actual: Any) = {
        if (expected == actual) {
            p("Pass: '%s' equals '%s'".format(expected, actual))
        }
        else {
            p("Fail:'%s' s not '%s'".format(expected, actual))   
        }
    }

    def main(args: Array[String]) { 
        p("** Welcome to the Scalaz playground **") 

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

        assertEquals((List(1, 2, 3) |@| List(4)).apply { (a: Int, b: Int) => a + b }, List(5, 6, 7))

        assertEquals((List(1, 2, 3) <*> List((i: Int) => i + 4)), List(5, 6, 7))
        assertEquals((List(1, 2, 3).map(_ + 4)), List(5, 6, 7))

        assertEquals(List(1, 2, 3).maximum, Some(3))

        assertEquals(List(1, 2, 3).any(_ > 2), true)
        assertEquals(List(1, 2, 3).any(_ > 4), false)

        assertEquals(List(55).getOrElseM(List(Some(77))), List(77))

        List(1, 2, 3) |>| { println(_) }

        // assertEquals(
        //     List(1, 2, 3) =>> { xs => xs.head }, 
        //     List(1))
    } 
}
