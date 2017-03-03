// 1.1.4 Printable Library

trait Printable[A] {
  def format(value: A): String
}

object PrintDefaults {
  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String = value
  }
  implicit val intPrintable = new Printable[Int] {
    def format(value: Int): String = value.toString
  }
}

object Print {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)
  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(format(value)(printable))
}

case class Cat(name: String, age: Int, color: String)

object PrintCats {
  implicit val catPrintable = new Printable[Cat] {
    def format(value: Cat): String =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object ObjectMain extends App {
  import PrintCats._

  val cat = Cat("Scruffles", 666, "Eldritch")

  Print.print(cat)
}

object PrintSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit printable: Printable[A]): String =
      printable.format(value)
    def print(implicit printable: Printable[A]): Unit =
      println(printable.format(value))
  }
}

object SyntaxMain extends App {
  import PrintCats._
  import PrintSyntax._

  val cat = Cat("Jingles", 777, "Red")

  cat.print
}

// 1.2.5 Cat Show

object Cat {
  import scalaz.Show

  implicit val catShow = Show.shows { cat: Cat =>
    s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }
}

object ShowMain extends App {
  import scalaz.syntax.show._

  val cat = Cat("Fluffy", 1, "white")
  println(cat.shows)
}

// 1.3.5 Equality, Liberty, and Felinity

object EqualCat {
  import scalaz.Equal

  implicit val catEqual = Equal.equal[Cat] { (cat1, cat2) =>
    import scalaz.syntax.equal._
    import scalaz.std.anyVal._
    import scalaz.std.string._

    cat1.name === cat2.name &&
    cat1.age === cat2.age &&
    cat1.color === cat2.color
  }
}

object EqualMain extends App {
  import scalaz.syntax.equal._
  import scalaz.std.option._
  import EqualCat._

  val cat1 = Cat("Garfield",   35, "orange and black")
  val cat2 = Cat("Heathcliff", 30, "orange and black")

  val optionCat1: Option[Cat] = Some(cat1)
  val optionCat2: Option[Cat] = None

  println(s"cat1 === cat2: ${cat1 === cat2}")
  println(s"cat1 =/= cat2: ${cat1 =/= cat2}")

  println(s"optionCat1 === optionCat2: ${optionCat1 === optionCat2}")
  println(s"optionCat1 =/= optionCat2: ${optionCat1 =/= optionCat2}")
}
