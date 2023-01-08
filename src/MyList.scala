import scala.annotation.tailrec

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]


object MyList extends App {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: MyList[A]): MyList[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }
  def setHead[A](as: MyList[A], x: A): MyList[A] = as match {
    case Nil => sys.error("cannot set head of empty list")
    case Cons(h, t) => Cons(x, t)
  }
  def drop[A](as: MyList[A], n: Int): MyList[A] = as match {
    case Nil => Nil
    case Cons(_, _) if n == 0 => as
    case Cons(h, t) => drop(t, n - 1)
  }

  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = as match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => as
  }

  def dropWhile2[A](as: MyList[A])( f: A => Boolean): MyList[A] = as match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => as
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => sys.error("Init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  val l1 = Cons(1, Cons(2, Cons(3, Nil)))
  dropWhile(l1, (x: Int) => x < 3)
  dropWhile2(l1)(x => x < 3)

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sumFoldRight(as: MyList[Int]): Int =
    foldRight(as, 0)((a, b) => a + b)

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0)( (a, b) => b + 1)

  def sumWithFoldLeft(as: MyList[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productWithFoldLeft(as: MyList[Int]): Int =
    foldLeft(as, 1)(_ * _)

  def appendViaFoldLeft[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(a1, a2)((b, a) => Cons(a, b))

  def lengthViaFrFl[A](as: MyList[A]): Int =
    foldRightViaFoldLeft(as, 0)((a, b) => b + 1)

  def foldRightViaFoldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  def doubleToString(as: MyList[Double]): MyList[String] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
    concatenate(map(l)(f))

  def concatenate[A](as: MyList[MyList[A]]): MyList[A] =
    foldLeft(as, MyList[A]())((b, a) => append(b, a))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def addTwoLists(as: MyList[Int], bs: MyList[Int]): MyList[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(h + h1, addTwoLists(t, t1))
  }

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(f(h, h1), zipWith(t, t1)(f))
  }


  @tailrec
  def foldLeft[A, B](l: MyList[A], acc: B)(f: (B, A) => B): B = l match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h))(f)
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(h, t) if (f(h)) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
  }

  def reverse[A](l: MyList[A]): MyList[A] =
    foldLeft(l, MyList[A]())((acc, h) => Cons(h, acc))

  def addOne(l: MyList[Int]): MyList[Int] =
    foldLeft(l, MyList[Int]())((b, a) => Cons(a + 1, b))

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    def loop[A](sup1: MyList[A], sub1: MyList[A]): Boolean = (sup1, sub1) match {
      case (Nil, Nil) => true
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h, t), Cons(h1, t1)) if h == h1 => loop(t, t1)
      case (Cons(h, t), Cons(h1, t1)) => loop(t, sub)
    }
    loop(sup, sub)
  }


  val l = foldRight(Cons(1, Cons(2, Cons(3, Cons(1, Cons(3, Nil))))), Nil: MyList[Int])(Cons(_, _))
  val lx = foldRight(Cons(1, Cons(3,  Nil)), Nil: MyList[Int])(Cons(_, _))

  val l2 = foldRightViaFoldLeft(Cons(1, Cons(2, Cons(3, Cons(1, Cons(3, Nil))))), Nil: MyList[Int])(Cons(_, _))
  val l3 = foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil: MyList[Int])((b, a) => Cons(a, b))
  val m = foldRight(Cons(1.0, Cons(2.1, Cons(3.4, Nil))), Nil: MyList[Double])(Cons(_, _))
  println(l)
  println(l2)
  println(l3)
  println(length(l))
  println(lengthViaFrFl(l))
  println(sumWithFoldLeft(l))
  println(sumFoldRight(l))
  println(productWithFoldLeft(l))
  println(appendViaFoldLeft(l, l))
  println(append(l, l))
  val lc = Cons(l, Cons(l, Nil))
  println(concatenate(lc))
  println(addOne(l))
  println(doubleToString(m))
  println(map(l)(_ * 4))
  println(filter(l)(_ < 2))
  println(addTwoLists(l,l))
  println(zipWith(l,l)(_ * _))
  println(hasSubsequence(l, lx))
}