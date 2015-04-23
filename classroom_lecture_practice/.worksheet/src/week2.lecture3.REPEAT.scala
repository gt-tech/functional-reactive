package week2.lecture3

object REPEAT {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(83); 
  println("Welcome to the Scala worksheet");$skip(125); 
  
  def WHILE(condition: => Boolean )(command: Unit): Unit = {
  if ( condition )
  command
  WHILE(condition)(command)
  };System.out.println("""WHILE: (condition#44651: => Boolean#2478)(command#44652: Unit#2583)Unit#2583""")}
  
  }
}
