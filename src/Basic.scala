object Basic extends App {

  import MyTree._

  println("Hllel")

  val b = Branch(Branch(Leaf(3), Leaf(5)), Leaf(15))
  println(size(b))

  println(maximum(b))

  println(depth(b))

  println(fold(b)((a: Int) => a)((c: Int, d: Int) => c + d))

  println(maximumViaFold(b))
}
