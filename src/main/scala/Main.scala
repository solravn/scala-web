import cats.data.Kleisli
import cats.effect._
import cats.implicits._
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import io.circe.syntax._
import io.circe.generic.auto._
import ru.pimpay._

object Main extends IOApp {

  val helloWorldService: Kleisli[IO, Request[IO], Response[IO]] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name => Ok(s"Hello, $name ~!!s dsasfsafas !!!!")

    case (POST|GET) -> Root / "db" => {
      Thread.sleep(4000)
      val dbS = ServiceState("dbm", active = true, "8080")
      Ok(dbS.asJson.noSpaces)
    };

    case GET -> Root / "api" => {
      Thread.sleep(4000);
      val apiS = ServiceState("platform.api", active = false, "4028")
      Ok(apiS.asJson.noSpaces)
    }

    case GET -> Root / "health" => {
      val dbIO  = pimpay.fetchUrl("http://localhost:8080/db")
      val apiIO = pimpay.fetchUrl("http://localhost:8080/api")
      val res   = for {
        dbFiber    <- dbIO.start
        apiFiber   <- apiIO.start
        dbContent  <- dbFiber.join
        apiContent <- apiFiber.join
        db         <- pimpay.parseHealthCheckContent(dbContent)
        api        <- pimpay.parseHealthCheckContent(apiContent)
      } yield s"db: $db\r\napi: $api"

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
