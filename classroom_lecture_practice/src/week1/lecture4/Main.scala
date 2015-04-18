package week1.lecture4

object Main extends App {

  import Generators._
  import TreeGen._
  
  val bool = booleans.generate
  println("Random generated boolean -> " -> bool)
  
  val intValue = integers.generate
  println("Random generated integers -> " -> intValue)
  
  val treeValue = trees.generate
  println("Random generated tree -> " -> treeValue)
}