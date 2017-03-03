// Exercise 5.3

import scala.concurrent.{Future, ExecutionContext}
import scalaz.EitherT

object FutureEithers {
  type FutureEither[A] = EitherT[Future, String, A]
}

object FutureEitherMain extends App {
  import scalaz.std.scalaFuture._
  import scalaz.syntax.monadError._
  import FutureEithers._
  import scala.concurrent.ExecutionContext.Implicits.global

  val loadAverages = Map(
    "a.example.com" -> 0.1,
    "b.example.com" -> 0.5,
    "c.example.com" -> 0.2
  )

  def getLoad(hostname: String): FutureEither[Double] = {
    import scalaz.syntax.monad._
    loadAverages.get(hostname) match {
      case Some(value) => value.point[FutureEither]
      case None => s"host ${hostname} unreachable".raiseError[FutureEither, Double]
    }
  }

  def getMeanLoad(hostnames: List[String]): FutureEither[Double] = {
    import scalaz.std.list._
    import scalaz.syntax.traverse._

    hostnames.length match {
      case 0 => s"no hostnames provided".raiseError[FutureEither, Double]
      case n => hostnames.map(getLoad).sequence.map(_.foldLeft(0.0)(_ + _) /n)
    }
  }

  def report[A](input: FutureEither[A]): Unit = {
    import scala.concurrent.Await
    import scala.concurrent.duration._

    Await.result(input.run, 2.seconds).fold(
      msg => println(s"[FAIL] ${msg}"),
      ans => println(s"[DONE] ${ans}")
    )
  }

  println(report(getLoad("a.example.com")))
  println(report(getLoad("example.com")))
  println(report(getMeanLoad(List("a.example.com", "b.example.com"))))
}
