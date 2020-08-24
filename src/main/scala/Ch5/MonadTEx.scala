package Ch5

import cats.data.EitherT
import cats.syntax.applicative._
import cats.instances.future._

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object MonadTEx {

  // defined type alias Response
  type Response[A] = Future[Either[String, A]]

  type FutureEitherResponse[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): FutureEitherResponse[Int] = {
    val powerLevels = Map( "Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)
    if(powerLevels.contains(autobot))
      powerLevels(autobot).pure[FutureEitherResponse]
    else
      //"autobot not available".pure[FutureEitherResponse]
      EitherT.left(Future(s"$autobot not available"))
  }

  def canSpecialMove(ally1: String, ally2: String): FutureEitherResponse[Boolean] = {
    val totalpower = for {
      x <- getPowerLevel(ally1)
      y <- getPowerLevel(ally2)
    } yield (x + y) > 15
    totalpower
    //Await.result(totalpower.value, 2 seconds) match {
    //  case Right(t: Int) if (t >= 15) => true.pure[FutureEitherResponse]
    //  case Right(t: Int) if (t < 15) => false.pure[FutureEitherResponse]
    //  case Left(error:String) => EitherT.left(Future(s"$error"))
    //}
  }

  def tacticalReport(ally1: String, ally2: String): String = {

    Await.result(canSpecialMove(ally1,ally2).value, 2 seconds) match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Left(error: String) => s"Comms error: $error"

    }
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Hot Rod", "Bumblebee"))
    println(tacticalReport("Jazz", "Ironhide"))
  }




}
