package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal { math.pow(b(), 2) - (4 * a() * c()) }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      var result: Set[Double] = Set()
      // (-b ± √Δ) / (2a)
      //
      // if delta is negative, the set of roots is empty
      // if delta is zero, the set of roots has one member
      // if delta is greater than zero, the set of roots has two members
      delta() match {
        case 0 => result = result.+ { (b() * -1) / (2 * a()) }
        case x if (x > 0) => result = result.+(
          ((b() * -1) + math.sqrt(x)) / (2 * a()),
          ((b() * -1) - math.sqrt(x)) / (2 * a()))
        case _ =>
      }
      result
    }
  }
}
