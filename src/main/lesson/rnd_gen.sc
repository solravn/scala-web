import shapeless._
import shapeless.:+:
import shapeless.CNil
import scala.reflect.runtime.universe._
import scala.util.Random

// ADT
case class Point(x:Int, y:Int)
case class Color(c:String)  // сделайте одно из нескольких цветов
case class Pixel(pos:Point, col: Color, opacity: Option[String] = None)
case class Bitmap(pixels: Vector[Pixel])

// Generator
trait Rnd[A] {
  def run(): A
}

// Primitives
implicit def intRnd: Rnd[Int] = () => Random.nextInt
implicit def strRnd: Rnd[String] = () => List.tabulate(10)(
  _ => Random.alphanumeric.filter(_.isLetter).head
).mkString

// Domain
implicit def colorRnd: Rnd[Color] = () => {
  val colors = List("red", "blue", "orange")
  Color(colors(Random.nextInt(colors.length)))
}

// Products
implicit def optionRnd[A : Rnd]: Rnd[Option[A]] =
  () => Option(implicitly[Rnd[A]].run())
implicit def vectorRnd[A : Rnd]: Rnd[Vector[A]] =
  () => Vector.fill(10)(implicitly[Rnd[A]].run)

// Shapeless
implicit val hnilRnd: Rnd[HNil] = () => HNil
implicit def hlistRnd[H, T <: HList]
  (implicit rndHead: Rnd[H], rndTail: Lazy[Rnd[T]]): Rnd[H :: T] =
    () => rndHead.run() :: rndTail.value.run()
implicit def genericRnd[A, R <: HList]
  (implicit gen: Generic.Aux[A,R], rndRepr: Lazy[Rnd[R]]): Rnd[A] =
    () => gen.from(rndRepr.value.run())

def random[A : Rnd]: A = implicitly[Rnd[A]].run()

random[Point]
random[Pixel]
//Generic[Option[Int]]
//Generic.materialize[Some[Int], Int :: HNil]
random[Bitmap]
//random[(String,String)].apply()
//random[Some[String]].apply()
//random[(Int, String, Int)].apply()
//random[((String, Int), Int)].apply()

//random[Pixell].apply()(
//  Generic.materialize[Pixell, Point :: HNil],
//  hlistRnd(
//    genericRnd(
//      Generic.materialize[Point, Int :: Int :: HNil],
//      lazily[Rnd[Int :: Int :: HNil]](
//        hlistRnd(
//          intRnd,
//          lazily[Rnd[Int :: HNil]](
//            hlistRnd(
//              intRnd,
//              lazily[Rnd[HNil]](hnilRnd)
//            )
//          )
//        )
//      )
//    ),
//    lazily[Rnd[HNil]](hnilRnd)
//  )
//)
