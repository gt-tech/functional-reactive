package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (key, value) => key -> Signal(eval(value(), namedExpressions)) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x)   => x
      case Ref(ref)     => eval(getReferenceExpr(ref, references), references)
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  
  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {

    def impl(n: String, visited: Set[String]): Expr = {
      val e: Expr = references.get(n).fold[Expr] { Literal(Double.NaN) } { exprSignal => exprSignal() }

      // the purpose of this function is to ensure that final expression does not have any reference
      // so function recursively de-normalized every argument until base condition of Literal is reached
      // if anywhere in that a value is seen more than once thru visited contains check, immediately
      // a NaN is returned.
      def denormalize(expr: Expr, visited: Set[String]): Expr = {
        expr match {
          case Literal(x)   => expr
          case Ref(ref)     => if (visited.contains(ref)) Literal(Double.NaN) else impl(ref, visited.+(ref))
          case Plus(a, b)   => Plus(denormalize(a, visited), denormalize(b, visited))
          case Minus(a, b)  => Minus(denormalize(a, visited), denormalize(b, visited))
          case Times(a, b)  => Times(denormalize(a, visited), denormalize(b, visited))
          case Divide(a, b) => Divide(denormalize(a, visited), denormalize(b, visited))
        }
      }
      denormalize(e, visited)
    }
    impl(name, Set(name))
  }
}
