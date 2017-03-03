// Exercise 3.3.4

import scala.language.higherKinds
import scalaz.Functor

sealed trait Result[+A]
final case class Success[A](value: A) extends Result[A]
final case class Warning[A](value: A, message: String) extends Result[A]
final case class Failure(message: String) extends Result[Nothing]

object ResultFunctor {
  def success[A](value: A): Result[A] = Success(value)
  def warning[A](value: A, message: String): Result[A] = Warning(value, message)
  def failure[A](message: String): Result[A] = Failure(message)

  implicit val successFunctor = new Functor[Result] {
    def map[A, B](value: Result[A])(func: A => B): Result[B] =
      value match {
        case Success(value) => Success[B](func(value))
        case Warning(value, message) => Warning[B](func(value), message)
        case Failure(message) => Failure(message)
      }
  }
}

object ResultFunctorMain extends App {
  import ResultFunctor._
  import scalaz.syntax.functor._

  println(success(1) map (_ + 1))
  println(warning(5, "this is a warning") map (_ + 1))
  println(failure[Int]("this is a failure") map (_ + 1))
}
