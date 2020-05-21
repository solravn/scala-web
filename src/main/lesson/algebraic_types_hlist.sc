
sealed trait HList
case class HCons[H,T <: HList](h: H, t: T) extends HList
case object HNil extends HList
type HNil = HNil.type

type ::[H,T <: HList] = HCons[H,T]

implicit class HListOps[T <: HList](t: T) {
  def ::[H](h: H): HCons[H,T] = HCons(h,t)
}

case class StatusSignal(id: Int, service: String, status: Boolean)

trait PimEncoder[A] {
  def encode(a: A): String
}

def buildPimEncoder[A](f: A => String): PimEncoder[A] = (a: A) => f(a)
implicit val intPimEncoder: PimEncoder[Int] = buildPimEncoder(_.toString)
implicit val strPimEncoder: PimEncoder[String] = buildPimEncoder(s => "\"" + s + "\"") // todo
implicit val boolPimEncoder: PimEncoder[Boolean] = buildPimEncoder(b => if (b) "T" else "F")
implicit val hnilPimEncoder: PimEncoder[HNil] = _ => "END"
implicit def hlistEncoder[H : PimEncoder,T <: HList : PimEncoder]: PimEncoder[H :: T] =
  (a: H :: T) => implicitly[PimEncoder[H]].encode(a.h) + ":" + implicitly[PimEncoder[T]].encode(a.t)

def encode[A <: HList : PimEncoder](a: A): String = implicitly[PimEncoder[A]].encode(a)

val tss = 19 :: "api" :: false :: HNil

def tuple2hlist[A,B](t: (A,B)): A :: B :: HNil = t._1 :: t._2 :: HNil
def tuple2hlist[A,B,C](t: (A,B,C)): A :: B :: C :: HNil = t._1 :: t._2 :: t._3 :: HNil

val t3 = (19, "api", false)
encode(tuple2hlist(t3))

val ss = StatusSignal(54, "api.pimpay.ru", status = true)

encode(tuple2hlist(StatusSignal.unapply(ss).get))

//// HW
trait Len[A] {
  def length(): Int
}

implicit def genLen[A]: Len[A] = () => 1
implicit val hnilLen: Len[HNil] = () => 0
implicit def hlistLen[H : Len, T <: HList : Len]: Len[H :: T] =
  () => implicitly[Len[H]].length + implicitly[Len[T]].length

def length[A <: HList : Len](hlist: A): Int =
  implicitly[Len[A]].length()


length( tuple2hlist( (1,2,3) ))
length( tuple2hlist( (1,"a",3) ))
length( tuple2hlist( (1,"a",3) ))
length( tuple2hlist( (1,"a",true) ))
length( tuple2hlist( (1,2,3) )) == 3
length( tuple2hlist( (2,3) )) == 2







