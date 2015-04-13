package week1.lecture2

object Json {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  abstract class Json
  case class JSeq(elems: List[Json]) extends Json
  case class JObj(bindings: Map[String, Json]) extends Json
  case class JNum(number: Double) extends Json
  case class JStr(value: String) extends Json
  case class JBool(bool: Boolean) extends Json
  case class JNull() extends Json

  def show(json: Json): String = json match {
    case JSeq(elems) => "[" + ((elems.map { e => show(e) }).mkString(", ")) + "]"
    case JObj(bindings) =>
      val obj = bindings map { case (k, v) => "\"" + k + "\"" + ":" + show(v) }
      "{" + obj.mkString(", ") + "}"
    case JNum(num)   => num.toString()
    case JStr(value) => "\"" + value + "\""
    case JBool(bool) => bool.toString()
    case JNull()     => "null"
  }                                               //> show: (json: week1.lecture2.Json.Json)String

  val demo = JObj(
    Map(
      "firstName" -> JStr("John"),
      "lastName" -> JStr("Doe"),
      "address" -> JObj(Map("street" -> JStr("123 fireway str"), "city" -> JStr("Fremont"), "state" -> JStr("CA"))),
      "registered" -> JBool(true),
      "license" -> JNull(),
      "phones" -> JSeq(List(JObj(Map("type" -> JStr("home"), "number" -> JNum(123456789))), JObj(Map("type" -> JStr("work"), "number" -> JNum(777777777)))))))
                                                  //> demo  : week1.lecture2.Json.JObj = JObj(Map(registered -> JBool(true), lice
                                                  //| nse -> JNull(), phones -> JSeq(List(JObj(Map(type -> JStr(home), number -> 
                                                  //| JNum(1.23456789E8))), JObj(Map(type -> JStr(work), number -> JNum(7.7777777
                                                  //| 7E8))))), lastName -> JStr(Doe), firstName -> JStr(John), address -> JObj(M
                                                  //| ap(street -> JStr(123 fireway str), city -> JStr(Fremont), state -> JStr(CA
                                                  //| )))))
  println(show(demo))                             //> {"registered":true, "license":null, "phones":[{"type":"home", "number":1.23
                                                  //| 456789E8}, {"type":"work", "number":7.77777777E8}], "lastName":"Doe", "firs
                                                  //| tName":"John", "address":{"street":"123 fireway str", "city":"Fremont", "st
                                                  //| ate":"CA"}}
}