package errorhandling

sealed trait MyEither[+E, +A]
case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
  def safeDivViaTry(x: Int, y: Int): MyEither[Exception, Int] = {
    Try(x / y)
  }
  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }
  def Try[A](a: => A): MyEither[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}
}