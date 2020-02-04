package ru.pimpay

import cats.effect._
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
}
