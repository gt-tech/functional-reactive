package week2.lecture3

object Loops {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  // =================== WHILE and DOWHILE LOOP ===================
  def WHILE(condition: => Boolean)(work: => Unit): Unit = {
    if (condition) {
      work
      WHILE(condition)(work)
    }
  }                                               //> WHILE: (condition#179131: => Boolean#2478)(work#179132: => Unit#2583)Unit#25
                                                  //| 83

  def DOWHILE(work: => Unit)(condition: => Boolean): Unit = {
    work
    if (condition) {
      DOWHILE(work)(condition)
    }
  }                                               //> DOWHILE: (work#179133: => Unit#2583)(condition#179134: => Boolean#2478)Unit#
                                                  //| 2583

  val a = Array(1, 2, 3)                          //> a  : Array#858[Int#1056] = Array(1, 2, 3)
  var i = 0                                       //> i  : Int#1056 = 0
  WHILE(i < a.length)({ println(i + " => " + a(i)); i += 1 })
                                                  //> 0 => 1
                                                  //| 1 => 2
                                                  //| 2 => 3
  i = 0
  DOWHILE({ println(i + " => " + a(i)); i += 1 })(i < a.length)
                                                  //> 0 => 1
                                                  //| 1 => 2
                                                  //| 2 => 3

  // =================== REPEAT LOOP ===================

  def REPEAT(work: => Unit) = new {
    def UNTIL(condition: => Boolean) {
      work
      if (condition) UNTIL(condition)
    }
  }                                               //> REPEAT: (work#179340: => Unit#2583)AnyRef#2716{def UNTIL#179357(condition#17
                                                  //| 9358: => Boolean#2478): Unit#2583}

  i = 0
  REPEAT {
    println(i + " => " + a(i)); i += 1
  } UNTIL (i < a.length)                          //> 0 => 1
                                                  //| 1 => 2
                                                  //| 2 => 3
}