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
import ru.pimpay.pimpay.MapTodoRepository

object Main extends IOApp {

  def json[A : Encoder](obj:A): IO[Response[IO]] =
    Ok(obj.asJson.noSpaces, Header("Content-Type", "application/json"))

  implicit class JsonOps[A : Encoder](obj:A) {
    def jsoned:String = obj.asJson.noSpaces
  }

  implicit class OptionOps[A : Encoder](opt:Option[A]) {
    def jsonedOr404(msg404:String = "херня"): IO[Response[IO]] =
      opt
        .map(t => Ok(t jsoned, Header("Content-Type", "application/json")))
        .getOrElse(NotFound(msg404))
  }

  val repo = new MapTodoRepository {}

  // set dummy todos
  (for {
    _ <- repo.append("Test1")
    _ <- repo.append("Test2")
  } yield ()).unsafeRunSync()

  val todoService = HttpRoutes.of[IO] {

    case GET -> Root / "todo" => for {
      todos    <- repo.findAll
      response <- Ok(todos.jsoned)
    } yield response

    case GET -> Root / "todo" / IntVar(id) => for {
      todo     <- repo findById id
      response <- todo jsonedOr404()
    } yield response

    case req @ POST -> Root / "todo" => for {
      msg      <- req.as[String]
      _        <- repo.append(msg)
      response <- Ok("Added")
    } yield response

    case req @ PATCH -> Root / "todo" / IntVar(id) / "complete" => for {
      todo     <- repo findById id
      _        <- repo.complete(id)
      response <- todo jsonedOr404()
    } yield response

  }.orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(todoService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
