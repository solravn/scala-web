import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.generic.semiauto._

sealed trait i18n_weight_format
case object Kg extends i18n_weight_format
case object Lbs extends i18n_weight_format

implicit def customEncoder(implicit format: i18n_weight_format): Encoder[Custom] =
  (a: Custom) => (format match {
    case Kg  => a.getWeight + "kg"
    case Lbs => a.getWeight * 2.25 + "lbs"
  }).asJson

class Custom(weight: Double) {
  def getWeight: Double = weight
}
case class Person(name: String, weight: Custom)

val p1 = Person("Tom", new Custom(80.0))

{
  implicit val f = Lbs
  p1.asJson
}
{
  implicit val f = Kg
  p1.asJson
}

case class Point(x: Int, y: Int)
case class Plot(map: Vector[Point])

implicit val pointEncoder: Encoder[Point] = (p: Point) =>
  (p.x -> p.y).asJson

val plot = new Plot(
  (for { i <- 1 to 5; j <- 1 to 5 } yield Point(i,j)).toVector)

val rawJson = plot.asJson.noSpaces

implicit val pointDecoder: Decoder[Point] = (c: HCursor) => {
  for {
    x <- c.downN(0).as[Int]
    y <- c.downN(1).as[Int]
  } yield Point(x,y)
}

decode[Plot](rawJson)

//case class Employee(name: String, salary: Money)
//case class Money(amount: Double, currency: String, rate: Double)
//
//val salary = Money(1203.30, "RUB", 1.2)
//val tom = Employee("Tom", salary)
//
//
//val rawEmp = tom.asJson
//val cloneTom = decode[Employee](rawEmp)
//
//val jsonAst = Json.obj(
//  "amount" -> Json.fromDouble(1203.03).get,
//  "currency" -> Json.fromString("RUB")
//)
//
//jsonAst.noSpaces


//val jsonRaw = """{"foo": {"bar": 1}, "x": "z"}"""
//
//val jsonAst = parse(jsonRaw)
//
//val fooBar = for {
//  json   <- parse(jsonRaw)
//  cursor = json.hcursor
//  foo    = cursor.downField("foo")
//  bar    = foo.downField("bar")
//  v      <- bar.as[Int]
//} yield v
