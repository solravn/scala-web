package ru.pimpay

import cats.effect._
import io.circe.parser._
import io.circe.generic.auto._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
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
    def complete(id: Int): IO[Unit]
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

    override def complete(id: Int): IO[Unit] = IO {
      mmap(id) = mmap(id).copy(status = Complete)
      ()
    }
  }
}
