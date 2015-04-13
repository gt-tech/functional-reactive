package week1.lecture2

object PartialPatternMatches {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(98); 
  println("Welcome to the Scala worksheet");$skip(85); 
  
  
  val f: PartialFunction[String, String] = {case "ping" => "f ---> " + "pong"};System.out.println("""f  : PartialFunction[String,String] = """ + $show(f ));$skip(83); val res$0 = 
  
  if ( f.isDefinedAt("ping") ) f("ping") else println("f doesn't support ping");System.out.println("""res0: Any = """ + $show(res$0));$skip(77); val res$1 = 
  if ( f.isDefinedAt("xyz") ) f("xyz") else println("f doesn't support xyz");System.out.println("""res1: Any = """ + $show(res$1))}
}
