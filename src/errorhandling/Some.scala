package errorhandling

case class Some[+A](get: A) extends MyOption[A]
