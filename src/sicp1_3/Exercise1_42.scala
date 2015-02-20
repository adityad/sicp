package sicp1_3

object Exercise1_42 {

  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    (x: Int) => f(g(x))
  }

  def nthrepeated(f: Int => Int, n: Int): Int => Int = {
    def nthrepeatedHelper(g: Int => Int, h: Int): Int => Int = {
      if (h <= 0) g
      else nthrepeatedHelper(compose(f, g), h - 1)
    }

    nthrepeatedHelper(f, n-1)
  }
}