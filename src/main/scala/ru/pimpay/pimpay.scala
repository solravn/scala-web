package ru.pimpay

import java.io.PrintWriter

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.collection.mutable.{Map => MMap}

package object pimpay {

  def fetchUrlF(url: String): Future[String] = Future {
    val source  = scala.io.Source.fromURL(new java.net.URL(url), "UTF-8")
    val content = source.mkString
    source.close
    content
  }

  def fetchUrl(url: String): IO[String] = IO.fromFuture(IO(fetchUrlF(url)))

  def parseHealthCheckContent(content: String): IO[String] = IO {
//    decode[ServiceStatus](content)
//      .map(s =>
//        if (s.active) s"${s.name} is OK, port: ${s.port}"
//        else s"${s.name} is not active, port: ${s.port}"
//      )
//      .getOrElse("Service is not responding")

    "ok"
  }

  sealed trait TodoStatus
  case object Pending extends TodoStatus
  case object Complete extends TodoStatus

  case class Todo(id: Int, msg: String, status: TodoStatus)

  case class TodoRequest(msg: String)

  trait TodoRepository {
    def findAll: IO[List[Todo]]
    def findById(id: Int): IO[Option[Todo]]
    def append(msg: String): IO[Unit]
    def complete(id: Int): IO[Option[Unit]]
  }

  class MapTodoRepository extends TodoRepository {

    val mmap: MMap[Int, Todo] = MMap()

    var nextId: Int = 1

    override def findAll: IO[List[Todo]] = IO(mmap.values.toList)

    override def findById(id: Int): IO[Option[Todo]] = IO(mmap.get(id))

    override def append(msg: String): IO[Unit] = IO {
      mmap(nextId) = Todo(nextId, msg, Pending)
      nextId = nextId + 1
      ()
    }

    override def complete(id: Int): IO[Option[Unit]] = IO ( for {
      todo <- mmap get id
      _    <- Option { mmap(id) = todo copy (status = Complete) }
    } yield ())
  }

  class LocalFileTodoRepository(val path: String) extends TodoRepository {

    type Id = Int

    protected def readContents: IO[String] = IO {
      val s = Source.fromFile(path)
      val r = s.mkString
      s.close
      r
    }

    protected def readDbAsMap: IO[Map[Id,Todo]] = for {
      raw <- readContents
      map <- IO.fromEither(decode[Map[Id,Todo]](raw))
    } yield map

    override def findAll: IO[List[Todo]] = for {
      map <- readDbAsMap
    } yield map.values.toList

    override def findById(id: Int): IO[Option[Todo]] = for {
      map <- readDbAsMap
    } yield map get id

    override def append(msg: String): IO[Unit] = for {
      todos    <- findAll
      maxId    = if (todos.isEmpty) 0 else (todos maxBy (_.id)).id
      todo     = Todo(maxId + 1, msg, Pending)
      newTodos = todos :+ todo
      jsonRaw  = newTodos.asJson.noSpaces
      _        <- IO { new PrintWriter(path, "UTF-8") { write(jsonRaw); close(); } }
    } yield ()

    override def complete(id: Int): IO[Option[Unit]] = for {
      todos    <- findAll
      map      = Map(todos map {t => t.id -> t}: _*)
      todo     = map get id
      updated  = todo map (_ => map.updated(id, todo.get.copy(status=Complete)))
      jsonRaw  = updated map (_.toList.asJson.noSpaces)
      result   <- if (jsonRaw.isDefined) {
        IO { new PrintWriter(path, "UTF-8") { write(jsonRaw.get); close(); }; Option(()) }
      } else IO.pure(Option.empty[Unit])
    } yield result
  }
}
