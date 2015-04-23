package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
    namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (key, value) => (key -> Signal(eval(value(), namedExpressions))) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x) => x
      case Ref(name) => /*eval(getReferenceExpr(name, references), references)*/
        {
          // val e = getReferenceExpr(name, references)
          // check the returned value for comparison for cyclic references
          // val result = e match {
          val result = getReferenceExpr(name, references) match {
            case Ref(new_name) if (new_name == name) => Literal(Double.NaN)
            case any                                 => any // whatever it is
          }
          eval(result, references)
        }
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
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
