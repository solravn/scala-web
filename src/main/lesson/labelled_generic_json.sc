import shapeless._
import shapeless.labelled.FieldType
import syntax.singleton._
import scala.reflect.runtime.universe.show
import scala.reflect.runtime.universe.reify

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

implicit def intDecoder: Decoder[Int] = r => r match {
  case JsNumber(n) => Right(n.toInt)
  case _ => { println(r);
    Left("JsNumber was expected") }
}
implicit def strDecoder: Decoder[String] = {
  case JsString(s) => Right(s)
  case _ => Left("JsString was expected")
}

implicit def arrDecoder[A](implicit valDec: Decoder[A]): Decoder[Vector[A]] = {
  case JsArray(vec) => vec
    .foldLeft(Right(Vector.empty[A]).asInstanceOf[Either[DecodeError,Vector[A]]]) {
      (acc, js) => valDec.decode(js) match {
        case Right(a) => acc.map(_ :+ a)
        case Left(e) => Left(e)
      }
    }
  case _ => Left("JsArray expected")
}

implicit def objDecoder[V](implicit valDec: Decoder[V]): Decoder[Map[String,V]] = {
  case JsObj(map) => map
    .foldLeft(Right(Map.empty[String,V]).asInstanceOf[Either[DecodeError,Map[String,V]]]) {
      (acc, t2) => valDec.decode(t2._2) match {
        case Right(v) => acc.map(_ + (t2._1 -> v))
        case Left(e) => Left(e)
      }
    }
  case _ => Left("JsObj expected")
}

// shapeless
implicit val hnilDec: Decoder[HNil] = _ => Right(HNil)
implicit def hconsDec[K <: Symbol, H, T <: HList]
  (implicit
     witness: Witness.Aux[K],
     headDec: Lazy[Decoder[H]],
     tailDec: Decoder[T]
  ): Decoder[FieldType[K, H] :: T] = (json: JSON) => {
    println(witness.value.name)
    headDec.value.decode(json) match {
      case Right(h) => {
        val k = witness.value.name
        println(h)
        ???
      }
      case Left(e) => Left(e)
    }
  }

// to CC
implicit def toCC[CC <: Product, HL <: HList]
  (implicit lgen: LabelledGeneric.Aux[CC,HL], enc: Lazy[Decoder[HL]]): Decoder[CC] =
    json => {
      enc.value.decode(json) match {
        case Right(hlist) => Right(lgen.from(hlist))
        case Left(e) => Left(e)
      }
    }

def decode[A : Decoder](json: JSON): Either[DecodeError,A] = implicitly[Decoder[A]].decode(json)

val jj = JsObj(Map("x" -> JsNumber(1.0), "y" -> JsNumber(2.0)))

decode[Point](jj)
//LabelledGeneric[Point]
//toCC[Point,FieldType[Symbol,Int] :: FieldType[String,Int] :: CNil].apply(jj)


//val jsn1 = JsArray(Vector(JsNumber(2), JsNumber(3)))
//val jsn2 = JsArray(Vector(JsNumber(3), JsString("asf")))
//val jsObj1 = JsObj(Map("kk" -> JsNumber(1)))
//val jsObj2 = JsObj(Map("kk" -> JsNumber(1), "kek" -> JsString("asd")))
//
//arrDecoder[Int].decode(jsn1)
//arrDecoder[Int].decode(jsn2)
//objDecoder[Int].decode(jsObj1)
//objDecoder[Int].decode(jsObj2)

//decode(JsObj(Map("a" -> JsNumber(1))))
//decode(JsObj(Map("p" -> JsNumber(2))))
//decode(encode(p1))

//decode[Point](p1)

