package playground

import errorhandling.MyEither

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

  List(1,2,3).headOption match {
    case Some(v) => println(s"head is $v")
    case _ => println("empty list")
  }

  case class Employee(name: String, department: String, manager: Option[String])
  val john = Employee("john", "accounts", Option("dick"))
  val fred = Employee("fred", "accounts", Option("dick"))
  val employees = Map("john" -> john, "fred" -> fred)
  def lookUpByName(name: String): Option[Employee] = employees.get(name)
  def getByName(n: String): Option[Employee] = {
    employees.get("none")
  }
  val d = lookUpByName("john").map(_.department).getOrElse("Default dept")
  println(s"department is $d")
  val m = lookUpByName("johnz").flatMap(_.manager)
  println(s"manager is $m")
  val b = lookUpByName("john").flatMap(a => getByName(a.department))
  println(s"managerc is $b")

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None else Option(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  println(s" MyEither 4 / 2: ${MyEither.safeDiv(4, 2)}")
}
