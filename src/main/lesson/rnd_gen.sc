import shapeless._
import shapeless.:+:
import shapeless.CNil
import scala.reflect.runtime.universe._
import scala.util.Random

// ADT
case class Point(x:Int, y:Int)
case class Color(c:String)  // сделайте одно из нескольких цветов
case class Pixel(pos:Point, c:Option[String] = None) // None = transparent
case class Pixell(pos:Point)
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

// Products
//implicit def optionGen[A]: Generic
implicit def someRnd[A : Rnd]: Rnd[Some[A]] = () => Some(implicitly[Rnd[A]].run())
implicit val noneRnd: Rnd[None.type] = () => None


// Shapeless
implicit val hnilRnd: Rnd[HNil] = () => HNil
implicit def hlistRnd[H, T <: HList]
  (implicit rndHead: Rnd[H], rndTail: Lazy[Rnd[T]]): Rnd[H :: T] =
    () => rndHead.run() :: rndTail.value.run()
implicit def genericRnd[A, R <: HList]
  (implicit gen: Generic.Aux[A,R], rndRepr: Lazy[Rnd[R]]): Rnd[A] =
    () => gen.from(rndRepr.value.run())

implicit def coproductRnd[L,R <: Coproduct]
  (implicit rndL: Rnd[L], rndR: Lazy[Rnd[R]]): Rnd[L :+: R] =
    () => ???

trait GenericRnd[A] {
  def apply[R <: HList]()(implicit gen: Generic.Aux[A,R], rndRepr: Rnd[R]): A
    = gen.from(rndRepr.run())
}
def random[A]: GenericRnd[A] = new GenericRnd[A]{}

random[Point]()
random[Pixell]()
//Generic[Option[Int]]
//Generic.materialize[Some[Int], Int :: HNil]
//random[Option[Int]]()
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

//show { reify {
//  random[(Int, Int)].apply()
//}}
//show { reify {
//  random[Pixell].apply()
//}}





