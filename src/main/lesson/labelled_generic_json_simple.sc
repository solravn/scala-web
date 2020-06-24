import shapeless._
import shapeless.labelled.FieldType
import syntax.singleton._
//import scala.reflect.runtime.universe.show
//import scala.reflect.runtime.universe.reify
import scala.reflect.runtime.universe.typeOf

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
case class Matrix(pp: Vector[Point])
case class Pixel(p: Point, c: String)
//LabelledGeneric[Point]

val p1 = encode(Point(1,2))
val px1 = encode(Pixel(Point(1,2), "red"))

/*************** decode ********************/

type DecodeError = String
type DecodeResult[A] = Either[DecodeError, A]

object Decoder {
  def unit[A](a: A): Decoder[A] = _ => Right(a)
}

trait Decoder[A] {
  self =>
  import Decoder._

  def decode(json: JSON): DecodeResult[A]

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = json => self.decode(json) match {
    case Right(a) => f(a).decode(json)
    case l @ Left(_) => l.asInstanceOf[DecodeResult[B]]
  }
  def map[B](f: A => B): Decoder[B] = self.flatMap(a => unit(f(a)))
}

implicit def intDecoder: Decoder[Int] = {
  case JsNumber(n) => Right(n.toInt)
  case _ => Left("JsNumber was expected")
}
implicit def strDecoder: Decoder[String] = {
  case JsString(s) => Right(s)
  case _ => Left("JsString was expected")
}

implicit def arrDecoder[A](implicit valDec: Decoder[A]): Decoder[Vector[A]] = {
  case JsArray(values) =>
    values.foldLeft(Right(Vector.empty[A]).asInstanceOf[DecodeResult[Vector[A]]]) {
      (acc, json) => valDec.decode(json) match {
        case Right(a) => acc.map(_ :+ a)
        case l @ Left(_) => l.asInstanceOf[DecodeResult[Vector[A]]]
      }
    }
  case _ => Left("JsArray expected")
}

object JsonHelper {
  def get(j: JSON, k: String): DecodeResult[JSON] = j match {
    case JsObj(fields) =>
      if (fields.contains(k)) Right(fields(k))
      else Left(s"Key $k not found!")
    case t @ _ => Left("JsObj expected, " + typeOf[t.type] + " given")
  }
}

// shapeless
implicit val hnilDec: Decoder[HNil] = _ => Right(HNil)
implicit def hconsDec[K <: Symbol, H, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   headDec: Lazy[Decoder[H]],
   tailDec: Decoder[T]
  ): Decoder[FieldType[K, H] :: T] =
  json => for {
    value  <- JsonHelper.get(json, witness.value.name)
    h      <- headDec.value.decode(value)
    t      <- tailDec.decode(json)
  } yield labelled.field[K](h) :: t

// to CC
implicit def genericDecoder[CC <: Product, HL <: HList]
  (implicit gen: LabelledGeneric.Aux[CC,HL], enc: Lazy[Decoder[HL]]): Decoder[CC] =
    enc.value.map(gen.from)

def decode[A : Decoder](json: JSON): DecodeResult[A] =
  implicitly[Decoder[A]].decode(json)

val jj = JsObj(Map("x" -> JsNumber(1.0), "y" -> JsNumber(2.0)))
val je = JsObj(Map("x" -> JsNumber(1.0), "z" -> JsNumber(2.0)))
val ja = JsArray(Vector())
val jm = JsObj(Map(
  "pp" -> JsArray(Vector(
    JsObj(Map("x" -> JsNumber(1), "y" -> JsNumber(3))),
    JsObj(Map("x" -> JsNumber(6), "y" -> JsNumber(4)))
  ))
))

decode[Point](p1)
decode[Matrix](jm)
//decode[Point](jj)
//decode[Point](je)

