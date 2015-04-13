package week1.lecture2

object PartialPatternMatches {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  val f: PartialFunction[String, String] = {case "ping" => "f ---> " + "pong"}
                                                  //> f  : PartialFunction[String,String] = <function1>
  
  if ( f.isDefinedAt("ping") ) f("ping") else println("f doesn't support ping")
                                                  //> res0: Any = f ---> pong
  if ( f.isDefinedAt("xyz") ) f("xyz") else println("f doesn't support xyz")
                                                  //> f doesn't support xyz
                                                  //| res1: Any = ()
}