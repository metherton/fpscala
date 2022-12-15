package playground

import scala.annotation.tailrec

object ScalaPlayground extends App {

  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, prev: Int, curr: Int): Int = {
      if (i <= 0) prev
      else go(i - 1, curr, prev + curr)
    }
    go(n, 0,1)
  }
  println(fib(4))

}
