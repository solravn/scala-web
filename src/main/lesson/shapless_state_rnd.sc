import shapeless._
import scala.util.Random

case class Point(x: Int, y: Int)
case class Color(c: String)
case class Pixel(pos: Point, c: Option[Color] = None)
case class Bitmap(pixels: Vector[Pixel])

// 1. type class
trait Rnd[A] {
  def gen(): A
}

// 2. summoner
def random[A](implicit rnd: Rnd[A]): A = rnd.gen

// 3. simple instances
implicit val intRnd: Rnd[Int] = () => Random.nextInt()
implicit val strRnd: Rnd[String] = () => Random.nextString(5)
implicit val colorRnd: Rnd[Color] = () => {
  val colors = List("red", "blue", "orange", "green")
  Color(colors(Random.nextInt(colors.length)))
}
implicit def optRnd[A : Rnd]: Rnd[Option[A]] = () =>
  if (Random.nextBoolean()) Option(implicitly[Rnd[A]].gen())
  else Option.empty[A]

implicit def vecRnd[A : Rnd]: Rnd[Vector[A]] = () =>
  List.fill(Random.nextInt(7))(implicitly[Rnd[A]].gen()).toVector

// shapeless
implicit val hnilRnd: Rnd[HNil] = () => HNil
implicit def hconsRnd[H, T <: HList](implicit hr: Rnd[H], tr: Rnd[T]): Rnd[H :: T] =
  () => hr.gen() :: tr.value.gen()
// coproduct
implicit def genRnd[T <: Product, R <: HList](implicit gen: Generic.Aux[T,R], rnd: Lazy[Rnd[R]]): Rnd[T] =
  () => gen.from(rnd.value.gen())

random[Pixel]
