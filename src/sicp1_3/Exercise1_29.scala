package sicp1_3
import scala.annotation.tailrec

object Exercise1_29 {

  def sumIntegers(a: Double, b:Double): Double = {
    if (a > b) 0
    else a + sumIntegers(a+1, b)
  }
  
  type Term = Double => Double
  type Next = Double => Double
  
  def summ(t: Term, a: Double, n: Next, b: Double): Double = {
    if (a>b) 0
    else t(a) + summ(t,n(a),n,b)
  }
  
  def summIter(t: Term, a: Double, n: Next, b: Double): Double = {
    @tailrec
    def summIterHelper(a:Double, result: Double): Double = {
      if(a>b) result
      else summIterHelper(n(a), t(a) + result)
    }
    
    summIterHelper(a, 0)
  }
}