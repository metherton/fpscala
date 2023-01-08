sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree extends App {
  def size[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def sizeViaFold[A](t: MyTree[A]): Int =
    fold(t, 0)((b,_) => b + 1)

  def maximum(t: MyTree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  def maxViaFold(t: MyTree[Int]): Int =
    fold(t, 0)((b, a) => b max a)

  def depth[A](t: MyTree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

//  def depthViaFold[A](t: MyTree[A]): Int =
//    fold(t, 0)((b: Int, a: Int) => b max a)

  def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: MyTree[A], acc: B)(f: (B, A) => B): B = t match {
    case Leaf(v) => f(acc, v)
    case Branch(l, r) => fold(r, fold(l, acc)(f))(f)
  }

  println(size(Leaf(1)))
  println(size(Branch(Leaf(1), Leaf(2))))
  println(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))))
  println(sizeViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))))
  println(maximum(Branch(Branch(Leaf(10), Leaf(6)), Branch(Leaf(3), Leaf(5)))))
  println(maxViaFold(Branch(Branch(Leaf(10), Leaf(6)), Branch(Leaf(3), Leaf(5)))))
  println(
    depth(
      Branch(Branch(Leaf(10), Leaf(6)),
             Branch(Leaf(3), Branch(Branch(Leaf(10), Leaf(6)), Leaf(10))))
    )
  )
  println(map(Leaf(1))(_ * 3))
  println(map(Branch(Leaf(1), Leaf(2)))(_ * 10))
  println(fold(Branch(Leaf(10), Leaf(2)), 2)((b, a) => a * b))

}
