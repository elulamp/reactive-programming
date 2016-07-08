package calculator

object Polynomial {


  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      math.pow(b(), 2.0) - 4.0 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {

      if (delta() < 0) {
        Set()
      } else {
        val negativeB = -1.0 * b()
        val doubleA = 2.0 * a()

        if (delta() == 0) {
          val x = negativeB / doubleA
          Set(x)
        } else {
          val sqrtDelta = math.sqrt(delta())
          val x0 = (negativeB + sqrtDelta) / doubleA
          val x1 = (negativeB - sqrtDelta) / doubleA

          Set(x0, x1)
        }

      }
    }
  }
}
