import scala.reflect.runtime.universe._

// Algebraic types (Product, Coproduct)
val t2: (Int,Int) = 3 -> 5

case class Point2D(x: Int, y: Int)
case class Point3D(x: Int, y: Int, z: Int)
case class Point3D2(p2d: Point2D, z: Int)

case class Human(name: String, age: Int)
case class Address(zipcode: Int, city: String)
case class Person(h: Human, address: Address)
// (String, Int, Int, String)

// Product = HList (Heterogeneous list) // Homogeneous
type ::[A,B] = (A,B)
type HNil = Unit
val HNil: HNil = ()

case class StatusSignal(id: Int, service: String, status: Boolean)

// Pim format: csv-like format for encoding data
// eg. StatusSignal(19, "api", true) = 19:"api":F:END
trait PimEncoder[A] {
  def encode(a: A): String
}

def buildPimEncoder[A](f: A => String): PimEncoder[A] = (a: A) => f(a)

implicit val intPimEncoder: PimEncoder[Int] = buildPimEncoder(_.toString)
implicit val strPimEncoder: PimEncoder[String] = buildPimEncoder(s => "\"" + s + "\"") // todo
implicit val boolPimEncoder: PimEncoder[Boolean] = buildPimEncoder(b => if (b) "T" else "F")
implicit val hnilPimEncoder: PimEncoder[HNil] = _ => "END"

implicit def hlistEncoder[H,T](implicit hEncoder: PimEncoder[H], tEncoder: PimEncoder[T]): PimEncoder[H :: T] =
  (a: H :: T) => hEncoder.encode(a._1) + ":" + tEncoder.encode(a._2)

def encode[A : PimEncoder](a: A): String = implicitly[PimEncoder[A]].encode(a)

encode(2)
encode(HNil)

val tss: Int :: String :: Boolean :: HNil = (19, ("api", (false, HNil)))


def tuple2hlist[A,B](t: (A,B)): A :: B :: HNil = (t._1, (t._2, HNil))
def tuple2hlist[A,B,C](t: (A,B,C)): A :: B :: C :: HNil = (t._1, (t._2, (t._3, HNil)))

val t3 = (19, "api", false)
encode(tuple2hlist(t3))
encode(tuple2hlist(2 -> 2))

val ss = StatusSignal(54, "api.pimpay.ru", status = true)

encode(tuple2hlist(StatusSignal.unapply(ss).get))

// HW
trait Counter[A] {
  def count(): Int
}

implicit def anyCounter[A]: Counter[A] = () => 1
implicit val hnilCounter: Counter[HNil] = () => 0
implicit def hlistCounter[H : Counter, T : Counter]: Counter[H :: T] =
  () => implicitly[Counter[H]].count + implicitly[Counter[T]].count

def length[A,B, AB >: A :: B : Counter](hlist: A :: B): Int =
  implicitly[Counter[AB]].count()



length( tuple2hlist( (1,2,3) ))
length( tuple2hlist( (1,"a",3) ))
length( tuple2hlist( (1,"a",3) ))
length( tuple2hlist( (1,"a",true) ))
length( tuple2hlist( (1,2,3) )) == 3
length( tuple2hlist( (2,3) )) == 2







