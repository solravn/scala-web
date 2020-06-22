import shapeless._
import scala.language.higherKinds

case class Config(id: Int, name: String)

val conf = Config(1, "kaz_post")

trait Rnd[A]


def a[B : Rnd[List[?]]]: Int = 1

Generic[Config].to(conf).select[String]
