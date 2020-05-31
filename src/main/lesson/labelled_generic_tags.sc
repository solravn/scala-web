import shapeless._
import syntax.singleton._

type Id = Int

def f(i: Id): Int = 1

type Tagged[S,T] = S with T
type @@[S,T] = Tagged[S,T]

implicit class tagged[A](v: A) {
  def tagged[T]: A @@ T = v.asInstanceOf[A @@ T]
}

case class Tagger[T]() {
  def apply[S](s: S): S @@ T = s.asInstanceOf[S @@ T]
}

trait IdTag
trait Age

val r: Int = 2.narrow
val v: Symbol = 'v
v.name

val x = 2.tagged[IdTag with Age]
