package calculator

import org.scalacheck._
import Gen._
import Arbitrary._
import scala.collection.immutable.TreeMap

object CalculatorProps extends Properties("Calculator") {

  // ======================== GENERATOR ============================
  val genCell: Gen[String] = Gen.oneOf("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

  val genLiteral: Gen[Literal] = for {
    i <- choose(1, 30)
  } yield Literal(i)

  val genRef: Gen[Ref] = for {
    name <- genCell
  } yield Ref(name.toString)

  val genExprPair: Gen[(Expr, Expr)] = for {
    a <- oneOf(genRef, genLiteral)
    b <- oneOf(genRef, genLiteral)
  } yield (a, b)

  val genPlus = for {
    ep <- genExprPair
  } yield Plus(ep._1, ep._2)

  val genMinus = for {
    ep <- genExprPair
  } yield Minus(ep._1, ep._2)

  val genTimes = for {
    ep <- genExprPair
  } yield Times(ep._1, ep._2)

  val genDivide = for {
    ep <- genExprPair
  } yield Divide(ep._1, ep._2)

  def genCalculatorNE(sz: Int): Gen[Map[String, Signal[Expr]]] = {
    mapOfN(sz, for {
      c <- genCell
      e <- oneOf(genLiteral, genRef, genPlus, genMinus, genTimes, genDivide)
    } yield ( c, Signal(e) ) )
  }
  // ======================== PROPERTY DEFINITIONS ============================

  def print(ne: Map[String, Signal[Expr]]): Unit = {
    println("Size -> " + ne.size); 
    ne/*TreeMap(ne.toSeq: _*)*/.foreach {
      case (k, v) => println( k + " = " + {
        v() match {
          case Literal(x)   => x
          case Ref(name)    => name
          case Plus(a, b)   => a + " + " + b
          case Minus(a, b)  => a + " - " + b
          case Times(a, b)  => a + " * " + b
          case Divide(a, b) => a + " / " + b
        }
      })
    }
  }
  //print(genCalculatorNE.sample.get)

//  property("validKey")  = Prop.forAll(genCalculatorNE(8)) { (ne: Map[String, Signal[Expr]]) => 
//    ne.forall{case(k, v) => k == "A" || k == "B" || k == "C" || k == "D" || k == "E" || k == "F" || k == "G" || k == "H" || k == "I" || k == "J"} }
//  
  property("computeValues") = Prop.forAll(genCalculatorNE(8)) { (ne: Map[String, Signal[Expr]]) =>
    print(ne)
    Calculator.computeValues(ne)
    true
  }
}