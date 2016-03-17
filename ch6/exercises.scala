// Exercise 6.3.4

object Result {
  import scalaz.syntax.std.string._

  sealed trait Result[+A]
  final case class Pass[+A](value: A) extends Result[A]
  final case class Fail(errors: List[String]) extends Result[Nothing]

  def readInt(str: String): Result[Int] = {
//    println(s"Reading $str")
    str.parseInt.disjunction.fold(
      exn => Fail(List(s"Error reading $str")),
      num => Pass(num)
    )
  }

  def sum2(a: Int, b: Int): Int = a + b

  import scalaz.Applicative

  val keepLeftApplicative = new Applicative[Result] {
    def ap[A, B](value: => Result[A])(func: => Result[A => B]): Result[B] =
      (value, func) match {
        case (Pass(a), Pass(f)) => point(f(a))
        case (Pass(a), Fail(m)) => Fail(m)
        case (Fail(m), _) => Fail(m)
      }

    def point[A](value: => A): Result[A] = Pass(value)
  }

  val keepAllApplicative = new Applicative[Result] {
    def ap[A, B](value: => Result[A])(func: => Result[A => B]): Result[B] =
      (value, func) match {
        case (Pass(a), Pass(f)) => point(f(a))
        case (Pass(a), Fail(n)) => Fail(n)
        case (Fail(m), Pass(f)) => Fail(m)
        case (Fail(m), Fail(n)) => Fail(m ++ n)
      }

    def point[A](value: => A): Result[A] = Pass(value)
  }

  import scalaz.Monad

  val monad = new Monad[Result] {
    def bind[A, B](value: Result[A])(func: A => Result[B]): Result[B] = value match {
      case Pass(a) => func(a)
      case Fail(m) => Fail(m)
    }

    def point[A](value: => A): Result[A] = Pass(value)
  }
}

// Exercise 6.4.2

object ApplicativeExercise extends App {
  import Result._

  val sum3 = (a: Int, b: Int, c: Int) => a + b + c
  val leftSum3 = keepLeftApplicative.lift3(sum3)
  val allSum3 = keepAllApplicative.lift3(sum3)
  val monadicSum3 = monad.lift3(sum3)

  val seqs = Seq(
    Seq("1", "2", "3"),
    Seq("1", "2", "def"),
    Seq("1", "abc", "3"),
    Seq("1", "abc", "def")
  )
  def testFn(f: (Result[Int], Result[Int], Result[Int]) => Result[Int]) = {
    seqs.map(args => {
      var results = args.map(readInt)
      args.reduce(_ + ", " + _) + " | " + results.map(_.toString).reduce(_ + ", " + _) + " | " + f(results(0), results(1), results(2))
    }).reduce(_ + "\n" + _)
  }

  println("keep left:")
  println(testFn(leftSum3))

  println("keep right:")
  println(testFn(allSum3))

  println("monad:")
  println(testFn(monadicSum3))
}


object ValidationExercise extends App {
  import scalaz.std.list._
  import scalaz.syntax.applicative._
  import scalaz.syntax.validation._
  import scalaz.syntax.either._
  import scalaz.syntax.std.boolean._
  import scalaz.syntax.std.list._
  import scalaz.syntax.std.option._
  import scalaz.syntax.std.string._
  import scalaz.Validation

  type Result[A] = Validation[List[String], A]

  case class User(name: String, age: Int)

  def readName(request: Map[String, String]): Result[String] =
    request.get("name").map(_.trim) match {
      case None => List("name must be specified").failure
      case Some("") => List("name must not be blank").failure
      case Some(name) => name.success
    }

  def readAge(request: Map[String, String]): Result[Int] = {
    val result = for {
      str <- request.get("age").toRightDisjunction(List("age must be specified"))
      int <- str.parseInt.disjunction.leftMap(_ => List("age must be an integer"))
      posInt <- (int < 0).fold(List("age must be non-negative").left, int.right)
    } yield posInt
    result.validation
  }

  def requestToUser(request: Map[String, String]): Result[User] =
    (readName(request) |@| readAge(request))(User.apply)

  println(requestToUser(Map("age" -> "")))
  println(requestToUser(Map("name" -> " ", "age" -> "1")))
  println(requestToUser(Map("name" -> "Alice", "age" -> "-256")))
  println(requestToUser(Map("name" -> "Bob", "age" -> "not a number")))
  println(requestToUser(Map("name" -> "Carol", "age" -> "255")))
}
