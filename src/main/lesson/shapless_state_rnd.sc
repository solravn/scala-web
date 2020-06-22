import shapeless._
//import scala.util.Random

/********************* STATE ***********************/
case class State[S,+A](run: S => (A,S)) {
  outer =>
  def map[B](f: A => B):State[S,B] = flatMap(a => State.unit[S](f(a)))
  def flatMap[B](f: A => State[S,B]):State[S,B] = State(s => {
    val (a, s2) = outer.run(s)
    f(a).run(s2)
  })
  def combine[B,C](sb:State[S,B])(f: (A,B) => C):State[S,C] = for {
    a <- outer
    b <- sb
  } yield f(a,b)
}

object State {
  case class PartialAppliedState[S]() {
    def apply[A](v:A):State[S,A] = State(v -> _)
  }
  def unit[S]: PartialAppliedState[S] = PartialAppliedState()
  def sequence[S,A](l:List[State[S,A]]):State[S,List[A]] = traverse(l)(identity)
  def traverse[S,A,B](l:List[A])(f: A => State[S,B]):State[S,List[B]] = l.foldRight(unit[S](List.empty[B])) {
    (el, acc) => combine(f(el), acc)(_ +: _)
  }
  def combine[S,A,B,C](sa:State[S,A], sb:State[S,B])(f: (A,B) => C):State[S,C] = sa.combine(sb)(f)
  def flatten[S,A](ssa:State[S, State[S, A]]):State[S,A] = ssa.flatMap(identity)
  def get[S]: State[S,S] = State((s:S) => s -> s)
  def set[S](s:S): State[S, Unit] = unit[S](()).flatMap(_ => State(s => () -> s))
  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object Random {
  import State._

  type Seed = Long
  type R[A] = State[Seed,A]

  private def randomInt(seed: Seed): (Int, Seed) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    (newSeed >>> 16).toInt -> newSeed
  }

  def int: R[Int] = State(randomInt)
  def nonNegativeInt: R[Int] = int.map(_.abs)
  def double: R[Double] = nonNegativeInt map { _.toDouble / Int.MaxValue }
  def range(from: Int, to: Int): R[Int] = nonNegativeInt.map(i => {
    require(to > from)
    from + (i % (to - from))
  })
  def boolean: R[Boolean] = nonNegativeInt.map(_ % 2 == 1)
  def char: R[Char] = range(97, 122).map(_.toChar)
  def listOf[A](ra: R[A], n: Int): R[List[A]] = sequence(List.fill(n)(ra))
  def fixedStr(l: Int): R[String] = listOf(char, l) map (_.mkString)
}

/********************** RND ************************/
trait Rnd[A] {
  def gen(): State[Random.Seed,A]
}

// instances
implicit val intRnd: Rnd[Int] = () => Random.int
implicit val strRnd: Rnd[String] = () => Random.fixedStr(6)
implicit val colorRnd: Rnd[Color] = () => Random.fixedStr(6).map(Color(_))
implicit def optRnd[A : Rnd]: Rnd[Option[A]] = () => implicitly[Rnd[A]].gen().map(Option(_))
implicit def vecRnd[A : Rnd]: Rnd[Vector[A]] = () => Random.listOf(implicitly[Rnd[A]].gen(), 2).map(_.toVector)
// shapeless
implicit val hnilRnd: Rnd[HNil] = () => State.unit(HNil)
implicit def hconsRnd[H, T <: HList](implicit hr: Rnd[H], tr: Lazy[Rnd[T]]): Rnd[H :: T] = () => for {
  hv <- hr.gen()
  tv <- tr.value.gen()
} yield hv :: tv
// coproduct
implicit def genRnd[T <: Product, R <: HList](implicit gen: Generic.Aux[T,R], rnd: Lazy[Rnd[R]]): Rnd[T] =
  () => rnd.value.gen().map(gen.from)

/********************** RUN ************************/

def random[A](implicit rnd: Rnd[A]): Random.R[A] = rnd.gen()

case class Point(x: Int, y: Int)
case class Color(c: String)
case class Pixel(pos: Point, c: Option[Color] = None)
case class Bitmap(pixels: Vector[Pixel])

random[Bitmap].run(123)
random[Bitmap].run(123)
random[Bitmap].run(1244)
