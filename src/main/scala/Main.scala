import cats.data.Kleisli
import cats.effect._
import cats.implicits._
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import ru.pimpay._

object Main extends IOApp {

  val helloWorldService: Kleisli[IO, Request[IO], Response[IO]] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name => Ok(s"Hello, $name ~!!s dsasfsafas !!!!")
    case (POST|GET) -> Root / "db" => Thread.sleep(4000); Ok("DB is ok")
    case GET -> Root / "api" => Thread.sleep(4000); Ok("API is ok")
    case GET -> Root / "health" => {
      val dbIO  = pimpay.fetchUrl("http://localhost:8080/db")
      val apiIO = pimpay.fetchUrl("http://localhost:8080/api")
      val res   = for {
        dbFiber  <- dbIO.start
        apiFiber <- apiIO.start
        db       <- dbFiber.join
        api      <- apiFiber.join
      } yield s"db: $db, api: $api"

      Ok(res)
    }
  }.orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(helloWorldService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
