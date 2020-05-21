import shapeless._

case class SignalStatus(id: Int, name:String, status:String)
val ss = SignalStatus(1, "api.pimpay.ru", "ok")

trait Len[A] {
  def length(): Int
}

implicit def anyLen[A]: Len[A] = () => 1
implicit val hnilLen: Len[HNil] = () => 0
implicit def hlistLen[H : Len, T <: HList : Len]: Len[H :: T] =
  () => implicitly[Len[H]].length + implicitly[Len[T]].length

// summoner
def length[A, R <: HList](a:A)(implicit gen: Generic.Aux[A,R], len: Len[R]): Int = len.length()

length(ss)

case class Point(x: Int, y: Int)
//case class Pixel(pos: Point, color: String, fags: (Int, Int, String))

//length(Pixel(Point(1,2), "red", (0, 3, "zr")))
length(Point(1,2))
length((3, 5, 1))

// HW
//def length2[A, R <: HList](implicit gen: Generic.Aux[A,R], len: Len[R]): Int = len.length()


trait GenericLen[A] {
  def apply[R <: HList]()(implicit gen: Generic.Aux[A,R], l: Len[R]): Int = l.length()
}

def length2[A]: GenericLen[A] = new GenericLen[A]{}

length2[SignalStatus]() // 3
length2[(Int, Int)]()   // 2





