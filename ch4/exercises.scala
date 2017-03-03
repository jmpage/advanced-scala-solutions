// Exercise 4.2

object GettingFuncy {
  def flatMap[F[_], A, B](value: F[A])(func: A => F[B]): F[B] = ???
  def point[F[_], A](value: A): F[A] = ???
  def map[F[_], A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)((a) => point(func(a)))
}

// Exercise 4.3.5

import scala.language.higherKinds
import scalaz.Monad

sealed trait Result[+A]
final case class Success[A](value: A) extends Result[A]
final case class Warning[A](value: A, message: String) extends Result[A]
final case class Failure(message: String) extends Result[Nothing]

object ResultMonad {
  def success[A](value: A): Result[A] = Success(value)
  def warning[A](value: A, message: String): Result[A] = Warning(value, message)
  def failure[A](message: String): Result[A] = Failure(message)

  implicit val resultMonad = new Monad[Result] {
    def bind[A, B](value: Result[A])(func: A => Result[B]): Result[B] =
      value match {
        case Success(v) => func(v)
        case Warning(v, message) => func(v) match {
          case Success(v) => Warning(v, message)
          case Warning(v, m2) => Warning(v, s"${message}, ${m2}")
          case Failure(m2) => Failure(m2)
        }
        case Failure(message) => Failure(message)
      }
    def point[A](value: => A): Result[A] = Success(value)
  }
}

object ResultMonadMain extends App {
  import ResultMonad._
  import scalaz.syntax.monad._

  println(success(1) map (_ + 1))
  println(warning(5, "this is a warning") map (_ + 1))
  println(warning(5, "this is a warning") flatMap ((a) => warning(a + 1, "this is another warning")))
  println(failure[Int]("this is a failure") map (_ + 1))

  val forResult = for {
    a <- success(2)
    b <- warning(5, "warning")
  } yield a * b

  println(forResult)
}
