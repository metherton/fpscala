object MyModule extends App {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  println(isSorted(Array(1,2,5, 3), (a: Int, b: Int) => a > b))

  val lessThan = new Function2[Int, Int, Boolean]() {
    override def apply(v1: Int, v2: Int): Boolean = v1 < v2
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  val timesTwo = (a: Int) => a * 2
  val timesFour = (a: Int) => a * 4

  val timesEight = timesTwo andThen timesFour

}
