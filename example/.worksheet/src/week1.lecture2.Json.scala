package week1.lecture2

object Json {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(82); 
  println("Welcome to the Scala worksheet")

  abstract class Json
  case class JSeq(elems: List[Json]) extends Json
  case class JObj(bindings: Map[String, Json]) extends Json
  case class JNum(number: Double) extends Json
  case class JStr(value: String) extends Json
  case class JBool(bool: Boolean) extends Json
  case class JNull() extends Json;$skip(740); 

  def show(json: Json): String = json match {
    case JSeq(elems) => "[" + ((elems.map { e => show(e) }).mkString(", ")) + "]"
    case JObj(bindings) =>
      val obj = bindings map { case (k, v) => "\"" + k + "\"" + ":" + show(v) }
      "{" + obj.mkString(", ") + "}"
    case JNum(num)   => num.toString()
    case JStr(value) => "\"" + value + "\""
    case JBool(bool) => bool.toString()
    case JNull()     => "null"
  };System.out.println("""show: (json: week1.lecture2.Json.Json)String""");$skip(437); 

  val demo = JObj(
    Map(
      "firstName" -> JStr("John"),
      "lastName" -> JStr("Doe"),
      "address" -> JObj(Map("street" -> JStr("123 fireway str"), "city" -> JStr("Fremont"), "state" -> JStr("CA"))),
      "registered" -> JBool(true),
      "license" -> JNull(),
      "phones" -> JSeq(List(JObj(Map("type" -> JStr("home"), "number" -> JNum(123456789))), JObj(Map("type" -> JStr("work"), "number" -> JNum(777777777)))))));System.out.println("""demo  : week1.lecture2.Json.JObj = """ + $show(demo ));$skip(22); 
  println(show(demo))}
}
