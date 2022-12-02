package playground

object ScalaPlayground extends App {

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (h :: t, h2 :: t2) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case h :: t => hasSubsequence(t, sub)
  }

  println(s"hasSubsequence: ${hasSubsequence(List(1,3), List(2))}")

}
