package week1.lecture4

object TestUsingGenerator extends App {

  import Generators._
  
  def test[T](g: Generator[T], numOfTimes: Int = 100)(testEval: T => Boolean): Unit = {
    for ( i <- 0 until numOfTimes ) {
      val value = g.generate
      assert(testEval(value), "test failed for '" + value + "' value on '" + (i + 1) + "' time")
    }
    
    println("test ok '" + numOfTimes + "'")
  }
  
  test(integers, 500)(_ < 10e50)
}