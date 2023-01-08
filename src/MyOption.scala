import MyList.map

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    this map(Some(_)) getOrElse(ob)
  }

  def filter[B >: A](f: A => Boolean): MyOption[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
    case (None, None) => None
    case (None, _) => None
    case (_, None) => None
    //case Some(a1: A, b1: B) => Some(f(a1, b1))
  }

  def map2ViaFlatMap[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap( aa => b map( bb => f(aa, bb)))

  def sequence[A](a: MyList[MyOption[A]]): MyOption[MyList[A]] =
    a match {
      case Cons(h, t) => h flatMap( hh => sequence(t).map(a => Cons(hh, a)))
      case Nil => Some(Nil)
    }

  def traverse[A, B](a: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] = {
    sequence(map(a)(x => f(x)))
  }

  /*
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
   */

}

