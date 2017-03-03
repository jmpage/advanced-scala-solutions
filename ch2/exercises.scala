// Exercise 2.2

import scalaz.Monoid

object BooleanMonoids {
  implicit val and: Monoid[Boolean] = new Monoid[Boolean] {
    def append(b1: Boolean, b2: => Boolean) = b1 && b2
    def zero = true
  }
  implicit val or: Monoid[Boolean] = new Monoid[Boolean] {
    def append(b1: Boolean, b2: => Boolean) = b1 || b2
    def zero = false
  }
  implicit val xor: Monoid[Boolean] = new Monoid[Boolean] {
    def append(b1: Boolean, b2: => Boolean) = (b1 && !b2) || (!b1 && b2)
    def zero = false
  }
}

// Exercise 2.3

object SetMonoids {
  implicit def append[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def append(s1: Set[A], s2: => Set[A]) = s1 ++ s2
    def zero = Set.empty[A]
  }
  implicit def union[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def append(s1: Set[A], s2: => Set[A]) = s1 | s2
    def zero = Set.empty[A]
  }
}

// Exercise 2.4.6

object SuperAdderMain extends App {
  import scalaz.syntax.monoid._
  import scalaz.std.anyVal._
  import scalaz.std.list._

  def add(items: List[Int]): Int = {
    items.fold(mzero[Int])(_ |+| _)
  }

  println(add(List(0)))
  println(add(List(1)))
  println(add(List(1, 2, 3)))
}

//TODO: figure out why this doesn't work
object SuperAdderMain2 extends App {
  import scalaz.Monoid
  import scalaz.syntax.monoid._
  import scalaz.std.option._
  import scalaz.std.anyVal._
  import scalaz.std.list._

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(mzero[A]){_ |+| _}

  println(add(List(1, 2, 3)))
  println(add(List(some(1), some(2), some(3)))) // You need to use some instead of Some so that it knows what the type is
}

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val monoid = Monoid.instance[Order]((o1, o2) => {
    Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)
  }, Order(0, 0))
}

object SuperAdderMain3 extends App {
  import scalaz.syntax.monoid._
  import scalaz.std.anyVal._
  import scalaz.std.list._
  import scalaz.std.option._

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.fold(mzero[A])(_ |+| _)
  }

  println(add(List.empty[Order]))
  println(add(List(Order(1, 2))))
  println(add(List(Order(1, 2), Order(3, 4))))
}

// Exercise 2.4.7

object ListSyntax {
  import scalaz.syntax.monoid._

  implicit class FoldMapOps[A: Monoid](base: List[A]) {
    def foldMap: A =
      base.foldLeft(mzero[A])(_ |+| _)
  }
}

object FoldMain extends App {
  import scalaz.std.anyVal._
  import scalaz.std.list._
  import ListSyntax._

  println(List(1, 2, 3).foldMap)
}

object ListSyntax2 {
  import scalaz.syntax.monoid._

  implicit class FoldMapOps[A](base: List[A]) {
    def foldMap(implicit monoid: Monoid[A]): A =
      base.foldLeft(mzero[A])(_ |+| _)
  }
}

