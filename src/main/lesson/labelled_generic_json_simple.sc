import shapeless._
import shapeless.labelled.{FieldType, field}
import syntax.singleton._
//import scala.reflect.runtime.universe.show
//import scala.reflect.runtime.universe.reify
//import scala.reflect.runtime.universe.typeOf

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
val px1 = encode(Pixel(Point(1,2), "red"))

/*************** decode ********************/

type DecodeError = String

// *** Cursor ***

sealed trait Cursor {
  def focus: Option[JSON]
  def succeed: Boolean
  def keys: Option[Iterable[String]]
  def downField(k: String): Cursor
  def downArray: Cursor
  def next: Cursor
  def as[A](implicit d: Decoder[A]): Either[DecodeError,A] = d.tryDecode(this)
  def get[A](k: String)(implicit d: Decoder[A]): Either[DecodeError,A] = downField(k).as[A]
}

case class FailedCursor(err: DecodeError) extends Cursor {
  override def focus: Option[JSON] = None
  override def succeed: Boolean = false
  override def keys: Option[Iterable[String]] = None
  override def downField(k: String): Cursor = this
  override def downArray: Cursor = this
  override def next: Cursor = this
}

abstract class HCursor extends Cursor {
  def value: JSON
  override def succeed: Boolean = true

  override final def downField(k: String): Cursor = value match {
    case j @ JsObj(fields) =>
      if (fields.contains(k)) ObjectCursor(j,k)
      else FailedCursor(s"Key $k not found")
    case _ => FailedCursor("DownField failed: value is not an object")
  }

  override final def downArray: Cursor = value match {
    case a @ JsArray(values) if values.nonEmpty => ArrayCursor(a, values.indices.head)
    case _ => FailedCursor("DownArray failed: value is empty or not JsArray")
  }

  override final def keys: Option[Iterable[String]] = value match {
    case JsObj(fields) => Some(fields.keys)
    case _  => None
  }

  override def next: Cursor = FailedCursor("Can't next")
}
object HCursor {
  def fromJson(json: JSON): HCursor = TopCursor(json)
}

case class TopCursor(value: JSON) extends HCursor {
  override def focus: Option[JSON] = Some(value)
}
case class ObjectCursor(obj: JsObj, key: String) extends HCursor {
  override def value: JSON = obj.fields(key)
  override def focus: Option[JSON] = if (obj.fields.contains(key)) obj.fields.get(key) else None
}
case class ArrayCursor(arr: JsArray, index: Int) extends HCursor {
  override def value: JSON = arr.values(index)
  override def focus: Option[JSON] =
    if (arr.values.indices.contains(index)) Some(arr.values(index)) else None
  override def next: Cursor = arr.values.indices.find(_ > index) match {
    case Some(nextIndex) => ArrayCursor(arr, nextIndex)
    case _ => FailedCursor("Array index is out of bound!")
  }
}

// *** Decoder ***

trait Decoder[A] {
  self =>
  def apply(c: HCursor): Either[DecodeError,A]

  def tryDecode(c: Cursor): Either[DecodeError,A] = c match {
    case h: HCursor => apply(h)
    case _ => Left("Attempt to decode value on failed cursor!")
  }

  def map[B](f: A => B): Decoder[B] = flatMap(a => Decoder.unit(f(a)));

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def apply(c: HCursor): Either[DecodeError,B] = self(c) match {
      case Right(a)    => f(a)(c)
      case l @ Left(_) => l.asInstanceOf[Either[DecodeError,B]]
    }
    override def tryDecode(c: Cursor): Either[DecodeError,B] = self.tryDecode(c) match {
      case Right(a)    => f(a).tryDecode(c)
      case l @ Left(_) => l.asInstanceOf[Either[DecodeError,B]]
    }
  }
}

object Decoder {
  def unit[A](v: A): Decoder[A] = _ => Right(v)
  def instance[A](f: HCursor => Either[DecodeError,A]): Decoder[A] = f(_)
}

implicit def intDecoder: Decoder[Int] = Decoder.instance(_.value match {
  case JsNumber(n) => Right(n.toInt)
  case _ => Left("JsNumber was expected")
})
implicit def strDecoder: Decoder[String] = Decoder.instance(_.value match {
  case JsString(s) => Right(s)
  case _ => Left("JsString was expected")
})

implicit def mapDecoder[V](implicit valDec: Decoder[V]): Decoder[Map[String,V]] = new Decoder[Map[String, V]] {
  override def apply(c: HCursor): Either[DecodeError, Map[String, V]] = c.value match {
    case JsObj(_) => decodeJsonObject(c)
    case _ => Left("JsObj expected")
  }
  def decodeJsonObject(c: HCursor): Either[DecodeError, Map[String, V]] =
    c.keys.get.foldLeft(Right(Map.empty[String, V]).asInstanceOf[Either[DecodeError, Map[String, V]]]) {
      (acc, key) => {
        val next = c.downField(key)
        if (next.succeed) {
          valDec.apply(next.asInstanceOf[HCursor]) match {
            case Right(v) => acc.map(_ + (key -> v))
            case l@Left(_) => l.asInstanceOf[Either[DecodeError, Map[String, V]]]
          }
        } else Right(Map.empty[String, V])
      }
    }
}

implicit def arrDecoder[A](implicit valDec: Decoder[A]): Decoder[Vector[A]] = new Decoder[Vector[A]] {
  override def apply(c: HCursor): Either[DecodeError,Vector[A]] = {
    val current = c.downArray
    if (current.succeed) decodeValues(current.asInstanceOf[HCursor])
    else Left("JsArray expected")
  }
  def decodeValues(c: HCursor): Either[DecodeError,Vector[A]] = valDec(c) match {
    case Right(a) =>
      val next = c.next
      if (next.succeed) decodeValues(next.asInstanceOf[HCursor]) match {
        case Right(b) => Right(a +: b)
        case l @ Left(_) => l.asInstanceOf[Either[DecodeError, Vector[A]]]
      }
      else Right(Vector(a))
    case l @ Left(_) => l.asInstanceOf[Either[DecodeError, Vector[A]]]
  }
}

// shapeless
implicit val hnilDec: Decoder[HNil] = Decoder.unit(HNil)
implicit def hconsDec[K <: Symbol, H, T <: HList]
  (implicit
   witness: Witness.Aux[K],
   headDec: Lazy[Decoder[H]],
   tailDec: Decoder[T]
  ): Decoder[FieldType[K, H] :: T] =
    Decoder.instance(c => for {
      h <- c.get[H](witness.value.name)(headDec.value)
      t <- tailDec(c)
    } yield field[K](h) :: t)

// to CC
implicit def genericDecoder[CC <: Product, HL <: HList]
  (implicit gen: LabelledGeneric.Aux[CC,HL], enc: Lazy[Decoder[HL]]): Decoder[CC] =
    Decoder.instance(c => enc.value(c) match {
      case Right(hl) => Right(gen.from(hl))
      case Left(e) => Left(e)
    })

def decode[A : Decoder](json: JSON): Either[DecodeError,A] =
  implicitly[Decoder[A]].apply(HCursor.fromJson(json))

val jj = JsObj(Map("x" -> JsNumber(1.0), "y" -> JsNumber(2.0)))
val je = JsObj(Map("x" -> JsNumber(1.0), "z" -> JsNumber(2.0)))
val ja = JsArray(Vector())

HCursor.fromJson(ja).as[Vector[Int]]

decode[Point](p1)
decode[Pixel](px1)
decode[Point](jj)
decode[Point](je)

