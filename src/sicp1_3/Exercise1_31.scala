package sicp1_3
import scala.annotation.tailrec

object Exercise1_31 {
  def fact(x: Int): Int = {
    @tailrec
    def factorial(y: Int, acc: Int): Int = {
      if (y == 0) acc
      else factorial(y - 1, acc * y)
    }

    factorial(x, 1)
  }

  type Term = Double => Double
  type Next = Double => Double

  def product(t: Term, a: Double, n: Next, b: Double): Double = {
    if (a > b) 1
    else t(a) * product(t, n(a), n, b)
  }

  def productIter(t: Term, a: Double, n: Next, b: Double): Double = {
    @tailrec
    def productIterHelper(a: Double, result: Double): Double = {
      if (a > b) result
      else productIterHelper(n(a), t(a) * result)
    }

    productIterHelper(a, 1)
  }
  
  def productIter2(t: Term, a: Double, n: Next, b: Double): BigDecimal = {
    @tailrec
    def productIterHelper(a: Double, result: BigDecimal): BigDecimal = {
      if (a > b) result
      else productIterHelper(n(a), t(a) * result)
    }

    productIterHelper(a, 1)
  }
}