package week2.lecture3

object Loops {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(83); 
  println("Welcome to the Scala worksheet");$skip(201); 

  // =================== WHILE and DOWHILE LOOP ===================
  def WHILE(condition: => Boolean)(work: => Unit): Unit = {
    if (condition) {
      work
      WHILE(condition)(work)
    }
  };System.out.println("""WHILE: (condition#179131: => Boolean#2478)(work#179132: => Unit#2583)Unit#2583""");$skip(135); 

  def DOWHILE(work: => Unit)(condition: => Boolean): Unit = {
    work
    if (condition) {
      DOWHILE(work)(condition)
    }
  };System.out.println("""DOWHILE: (work#179133: => Unit#2583)(condition#179134: => Boolean#2478)Unit#2583""");$skip(27); 

  val a = Array(1, 2, 3);System.out.println("""a  : Array#858[Int#1056] = """ + $show(a ));$skip(12); 
  var i = 0;System.out.println("""i  : Int#1056 = """ + $show(i ));$skip(62); 
  WHILE(i < a.length)({ println(i + " => " + a(i)); i += 1 });$skip(8); 
  i = 0;$skip(64); 
  DOWHILE({ println(i + " => " + a(i)); i += 1 })(i < a.length);$skip(195); 

  // =================== REPEAT LOOP ===================

  def REPEAT(work: => Unit) = new {
    def UNTIL(condition: => Boolean) {
      work
      if (condition) UNTIL(condition)
    }
  };System.out.println("""REPEAT: (work#179340: => Unit#2583)AnyRef#2716{def UNTIL#179357(condition#179358: => Boolean#2478): Unit#2583}""");$skip(10); 

  i = 0;$skip(75); 
  REPEAT {
    println(i + " => " + a(i)); i += 1
  } UNTIL (i < a.length)}
}
