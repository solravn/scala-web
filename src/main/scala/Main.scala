import cats.data.Kleisli
import cats.effect._
import cats.implicits._
import org.http4s.{Header, HttpRoutes, Request, Response}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.headers._
import org.http4s.server.blaze._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._
import ru.pimpay._

object Main extends IOApp {

  val helloWorldService: Kleisli[IO, Request[IO], Response[IO]] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name => Ok(s"Hello, $name ~!!s dsasfsafas !!!!")

    case (POST|GET) -> Root / "db" => {
      /* Thread.sleep(4000) */
      json(ServiceStatus("dbm1", "ok"))
    };

    case GET -> Root / "api" => {
//      Thread.sleep(4000);
      json(ServiceStatus("platform.api", "ok"))
    }

    case GET -> Root / "health" => {
      val dbIO  = pimpay.fetchUrl("http://localhost:8080/db")
      val apiIO = pimpay.fetchUrl("http://localhost:8080/api")
      val res   = for {
        dbFiber    <- dbIO.start
        apiFiber   <- apiIO.start
        dbRaw      <- dbFiber.join
        apiRaw     <- apiFiber.join
        db  = decode[ServiceStatus](dbRaw)
        api = decode[ServiceStatus](apiRaw)
      } yield json(
        Map(
          "api" -> api.getOrElse(ServiceStatus("api", "Failed")),
          "db" -> db.getOrElse(ServiceStatus("db", "Failed"))
        )
      )

      res.flatten
    }
  }.orNotFound

  def json[A : Encoder](obj: A): IO[Response[IO]] =
    Ok(obj.asJson.noSpaces, Header("Content-Type", "application/json"))

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(helloWorldService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
