import shapeless._
import scala.reflect.runtime.universe._
import scala.util.Random

// generator
trait Rnd[A] {
  def run(): A
}

// helpers
def randomInt(r: Range): Int = r(Random.nextInt(r.last - r.head + 1))

// primitives
implicit def intRnd: Rnd[Int] = () => randomInt { 1 to 9999 }
implicit def strRnd: Rnd[String] = () => List.fill(randomInt(3 to 15))(Random.nextPrintableChar).mkString
implicit def boolRnd: Rnd[Boolean] = () => Random.nextBoolean()
implicit def doubleRnd: Rnd[Double] = () => Random.nextDouble()

// products
implicit def optionRnd[A](implicit aRnd: Rnd[A], boolRnd: Rnd[Boolean]): Rnd[Option[A]] = () =>
  if (boolRnd.run()) Option(aRnd.run()) else Option.empty[A]
implicit def eitherRnd[E,A](implicit eRnd: Rnd[E], aRnd: Rnd[A], boolRnd: Rnd[Boolean]): Rnd[Either[E,A]] = () =>
  if (boolRnd.run()) Right(aRnd.run()) else Left(eRnd.run())
implicit def listRnd[A : Rnd]: Rnd[List[A]] = () =>
  List.fill(1 + Random.nextInt(randomInt(2 to 4)))(implicitly[Rnd[A]].run)
implicit def vectorRnd[A](implicit lr: Rnd[List[A]]): Rnd[Vector[A]] = () =>
  lr.run().toVector
implicit def mapRnd[K,V](implicit kr: Rnd[K], lr: Rnd[List[V]]): Rnd[Map[K,V]] = () =>
  lr.run().zipWithIndex.map(t2 => kr.run() -> t2._1).toMap

// shapeless
implicit val hnilRnd: Rnd[HNil] = () => HNil
implicit def hlistRnd[H, T <: HList]
  (implicit rndHead: Rnd[H], rndTail: Lazy[Rnd[T]]): Rnd[H :: T] =
    () => rndHead.run() :: rndTail.value.run()
implicit def genericRnd[A, R <: HList]
  (implicit gen: Generic.Aux[A,R], rndRepr: Lazy[Rnd[R]]): Rnd[A] =
    () => gen.from(rndRepr.value.run())

def random[A : Rnd]: A = implicitly[Rnd[A]].run()

// ADT
case class Point(x:Int, y:Int)
case class Color(c: String)
case class Pixel(pos:Point, col: Color, opacity: Option[String] = None)
case class Bitmap(pixels: Vector[Pixel])

implicit def colorRnd: Rnd[Color] = () => {
  val colors = List("red", "blue", "orange", "green")
  Color(colors(Random.nextInt(colors.length)))
}

random[Map[Int, Either[String, Pixel]]]
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
