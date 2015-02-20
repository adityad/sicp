package sicp1_3

import sicp1_3.Exercise1_29._;
import sicp1_3.Exercise1_31._;
import sicp1_3.Exercise1_35._;
import sicp1_3.Exercise1_42._;

object Main {
  def main(args: Array[String]) {
//    println("Integral cubes approximate")
//    def cube(n: Double): Double = n * n * n;
//
//    println(integral(cube, 0, 1, 0.01))
//    println(integral(cube, 0, 1, 0.001))
//    println(simpsons_integral(cube, 0, 1, 100))
//    println(simpsons_integral(cube, 0, 1, 100000))
//    println("Approx PI value: " + pi_approx(100000))
    
    println("Approx Golden Ratio: " + fixedPointGoldenRatio((x:BigDecimal) => (1+1/x)))
    
    def square(x:Int) = { x * x }
    println("Nth Repeated " + nthrepeated(square, 2)(5))
  }

  def integral(t: Term, a: Int, b: Int, dx: Double): Double = {
    def next(n: Double): Double = n + dx;

    summ(t, (a + dx / 2.0), next, b) * dx;
  }

  def simpsons_integral(t: Term, a: Int, b: Int, n: Int): Double = {
    val h = (1.0) * (b - a) / n;

    def next(cur: Double): Double = cur + h;
    def next2(cur: Double): Double = cur + 2 * h;

    (h / 3.0) * (t(a) + t(b) + 2 * (summIter(t, next(a), next, b - h) + summIter(t, next(a), next2, b - h)));
  }
  
  def pi_approx(n:Int): BigDecimal = {
    def term(x: Double): Double = x * x;
    def next(x: Double): Double = x + 2;
    
    8 * (productIter2(term,4,next,n)/productIter2(term,3,next,n)) / (n-1)
  }
}