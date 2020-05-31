import shapeless._
import shapeless.labelled.FieldType
import syntax.singleton._

sealed trait JSON
case object JsNull extends JSON
case class JsNumber(v: Double) extends JSON
case class JsString(s: String) extends JSON
case class JsBool(b: Boolean) extends JSON
case class JsArray(values: Vector[JSON]) extends JSON
case class JsObj(fields: Map[String,JSON]) extends JSON

def stringify(json:JSON):String = {
  def q(s:String):String = "\"" + s + "\"" // TODO: normal enquote
  json match {
    case JsNull        => "null"
    case JsBool(b)     => if (b) "true" else "false"
    case JsNumber(n)   => n.toString()
    case JsString(s)   => q(s)
    case JsArray(v)    => "[" + (v map {stringify} mkString ",") +"]"
    case JsObj(fields) => "{" + (fields map { case (k,v) => q(k) + ":" + stringify(v)} mkString ",") + "}"
  }
}

/*************** encode ********************/

trait JsonEnc[A] {
  def enc(a: A): JSON
}

// basic
implicit def numEnc[A : Numeric]: JsonEnc[A] = a => JsNumber(implicitly[Numeric[A]].toDouble(a))
implicit val strEnc: JsonEnc[String] = JsString(_)
implicit val boolEnc: JsonEnc[Boolean] = JsBool(_)

implicit def optionEnc[A : JsonEnc]: JsonEnc[Option[A]] =
  _.fold[JSON](JsNull)(implicitly[JsonEnc[A]].enc)

// shapeless
implicit val hnilEnc: JsonEnc[HNil] = _ => JsObj(Map())

implicit def hconsEnc[K <: Symbol, H, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   headEnc: Lazy[JsonEnc[H]],
   tailEnc: JsonEnc[T]
  ): JsonEnc[FieldType[K, H] :: T] =
    (hlist: FieldType[K, H] :: T) => {

      val h = headEnc.value.enc(hlist.head)
      val k = witness.value.name

      JsObj(Map(k -> h) ++ tailEnc.enc(hlist.tail).asInstanceOf[JsObj].fields)
    }

// from CC
implicit def fromCC[CC <: Product, LH <: HList]
  (implicit lgen: LabelledGeneric.Aux[CC,LH], enc: Lazy[JsonEnc[LH]]): JsonEnc[CC] =
    cc => enc.value.enc(lgen.to(cc))

// summoner
def encode[A : JsonEnc](a: A): JSON = implicitly[JsonEnc[A]].enc(a)

// run
val j1 = JsObj(Map("x" -> JsNumber(1), "y" -> JsNumber(2)))

case class Point(x: Int, y: Int)
case class Pixel(p: Point, c: String)
//LabelledGeneric[Point]

val p1 = encode(Point(1,2))
encode(Pixel(Point(1,2), "red"))

/*************** decode ********************/

type DecodeError = String

trait Decoder[A] {
  def decode(json: JSON): Either[DecodeError,A]
}
//class StrDecoder extends Decoder[String] {
//  override def decode(json: JSON): Either[DecodeError,String] = json match {
//    case JsString(s) => Right(s)
//    case _ => Left("String was expected")
//  }
//}
//case class NumDecoder() extends Decoder[Double] {
//  override def decode(json: JSON): Either[DecodeError,Double] = json match {
//    case JsNumber(n) => Right(n)
//    case _ => Left("Number was expected")
//  }
//}
implicit def intDecoder: Decoder[Int] = {
  case JsNumber(n) => Right(n.toInt)
  case _ => Left("Number was expected")
}

case class IntDecoder() extends Decoder[Int] {
  override def decode(json: JSON): Either[DecodeError,Int] = json match {
    case JsNumber(n) => Right(n.toInt)
    case _ => Left("Number was expected")
  }
}

case class ArrDecoder[A] extends Decoder[Vector[A]] {
  override def decode(json: JSON)(implicit valDec: Decoder[A]): Either[DecodeError, Vector[A]] = json match {
    case JsArray(values) => {
      val mapped = values.map(vj => valDec.decode(vj))

    }
    case _ => Left("Array was expected")
  }
}


def decodeArr(j: JSON): Either[DecodeError, Point] = {
  val d = ArrDecoder[Int]()
  d.decode(j).map(v => Point(v(0), v(1)))
}

decodeArr(JsArray(Vector(JsNumber(2), JsNumber(3))))

NumDecoder().decode(JsNumber(1))

//decode(JsObj(Map("a" -> JsNumber(1))))
//decode(JsObj(Map("p" -> JsNumber(2))))
//decode(encode(p1))

//decode[Point](p1)

