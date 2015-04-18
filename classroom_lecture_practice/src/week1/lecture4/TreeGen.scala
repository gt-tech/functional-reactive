package week1.lecture4

object TreeGen {

  import Generators._
  
  trait Tree
  
  case class Inner(left: Tree, right: Tree) extends Tree
  
  case class Leaf(x: Int) extends Tree
  
  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)
  
  
  def inners: Generator[Inner] = for {
    x <- trees
    y <- trees
  } yield new Inner(x, y)
  
  
  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if ( isLeaf) leafs else inners 
  } yield tree
  
}