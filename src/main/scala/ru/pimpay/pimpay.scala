package ru.pimpay

import cats.effect._
import io.circe.parser._
import io.circe.generic.auto._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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
}
