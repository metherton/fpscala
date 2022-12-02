package errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(v)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[E, C] = (this, b) match {
    case (Left(v), Left(w)) => Left(v)
    case (Right(v), Right(w)) => Right(f(v, w))
    case (Left(v), Left(_)) => Left(v)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e:Exception => Left(e)}
  }
}