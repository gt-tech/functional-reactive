package week1.lecture4

object Generators {

  trait Generator[+T] {
    
    self => // alias for this 
      
    def generate: T
    
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
    
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
    
  }
  
  val integers = new Generator[Int] {
    import scala.util.Random
    
    private val rand = Random;
    def generate: Int = rand.nextInt();
  }
  
  /*val cumbersome_booleans = new Generator[Boolean] {
    def generate: Boolean = integers.generate > 0
  }
  
  val cumbersome_pairs = new Generator[(Int, Int)] {
    def generate: (Int, Int)  = (integers.generate, integers.generate)
  }*/
  
 val booleans = for { i <- integers } yield (i >= 0)
  // expands do - booleans = integers map ( x = x > 0 )
  
  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    tt <- t
    uu <- u
  } yield (tt,uu)
  // expands to pairs = t flatmap{ tt => u map { uu => (tt, uu)}}
  
  
}