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

        assertEquals(List(1, 2, 3).maximum, 3)

        List(1, 2, 3) |>| { println(_) }
    } 
}
