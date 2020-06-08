import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._

val p = parse("""{"a": {"b":[1]}}""").getOrElse(Json.Null)

val c = HCursor.fromJson(p)


c.downField("a").downField("b").downArray