package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Var(
      if (delta() < 0) Set()
      else if (delta() == 0) Set(-b() / (2 * a()))
      else Set(
        (-b() - Math.sqrt(delta()))/ (2 * a()),
        (-b() + Math.sqrt(delta()))/ (2 * a())
      )
    )
  }
}
