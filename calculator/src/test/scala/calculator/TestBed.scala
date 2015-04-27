package calculator

object TestBed extends App {

  val input: Map[String, Signal[Expr]] = Map(("a" -> Signal(Literal(1.0))),
    ("b" -> Signal(Ref("a"))),
    ("c" -> Signal(Plus(Literal(10.0), Ref("b")))))

  val output: Map[String, Signal[Double]] = Calculator.computeValues(input);

  output.foreach{case(k , v) => println(k + " = " + v())}
}
