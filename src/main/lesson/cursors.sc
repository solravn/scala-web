
sealed trait JSON
case object JsNull extends JSON
case class JsNumber(v: Double) extends JSON
case class JsString(s: String) extends JSON
case class JsBool(b: Boolean) extends JSON
case class JsArray(values: Vector[JSON]) extends JSON
case class JsObj(fields: Map[String,JSON]) extends JSON

type DecodeError = String

sealed trait Cursor {

  def focus: Option[JSON]
  def succeed: Boolean

  def keys: Option[Iterable[String]]
  def downField(k: String): Cursor
  def downArray: Cursor
  def next: Cursor

  final def as[A](implicit d: Decoder[A]): Either[DecodeError,A] = d.tryDecode(this)
  final def get[A](k: String)(implicit d: Decoder[A]): Either[DecodeError,A] = downField(k).as[A]
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
    case _ => FailedCursor("Array is empty")
  }
}

trait Decoder[A] {
  self =>

  def apply(c: HCursor): Either[DecodeError,A]

  def tryDecode(c: Cursor): Either[DecodeError,A] = c match {
    case h: HCursor => apply(h)
    case _ => Left("Attempt to decode value on failed cursor")
  }

  def map[B](f: A => B): Decoder[B] = flatMap(a => Decoder.unit(f(a)));

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {

    final def apply(c: HCursor): Either[DecodeError,B] = self(c) match {
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

  final def apply[A](implicit instance: Decoder[A]): Decoder[A] = instance

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
  self =>
  override def apply(c: HCursor): Either[DecodeError,Vector[A]] = {
    val current = c.downArray
    if (current.succeed) decodeValues(current.asInstanceOf[HCursor])
    else Left("JsArray expected")
  }
  def decodeValues(c: HCursor): Either[DecodeError,Vector[A]] = valDec(c) match {
    case Right(a) =>
      val next = c.next
      if (next.succeed) {
        decodeValues(next.asInstanceOf[HCursor]) match {
          case Right(b) => Right(a +: b)
          case l @ Left(_) => l.asInstanceOf[Either[DecodeError, Vector[A]]]
        }
      } else Right(Vector(a))
    case l @ Left(_) => l.asInstanceOf[Either[DecodeError, Vector[A]]]
  }
}

def decode[A : Decoder](json: JSON): Either[DecodeError,A] =
  implicitly[Decoder[A]].apply(HCursor.fromJson(json))

val js1 = JsString("asd")
val jo1 = JsObj(Map("x" -> JsString("a")))
val jo2 = JsObj(Map("a" -> JsObj(Map("b" -> JsNumber(2), "c" -> JsNumber(3)))))
val jo3 = JsObj(Map("a" -> JsArray(Vector(JsNumber(2), JsNumber(3)))))
val ja1 = JsArray(Vector(JsNumber(2), JsNumber(3)))
val ja2 = JsArray(Vector())
val jt  = JsArray(Vector(JsObj(Map("x" -> JsString("a")))))

//decode(JsString("asf"))

HCursor.fromJson(ja2).as[Vector[Int]]
HCursor.fromJson(ja2).as[Map[String,Int]]
HCursor.fromJson(jt).as[Vector[Map[String,String]]]

HCursor.fromJson(jo1).as[Map[String,String]]

HCursor.fromJson(jo3).as[Map[String,Vector[Int]]]
HCursor.fromJson(jo3).as[Map[String,Vector[String]]]

HCursor.fromJson(jo2).downField("a").downField("g")
HCursor.fromJson(jo2).downField("a").downField("b").as[Int]

HCursor.fromJson(jo2).as[Map[String,Map[String,Int]]]
