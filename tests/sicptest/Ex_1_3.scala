package sicptest

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Ex_1_3 extends FunSuite {
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  import sicp1_3.Exercise1_29._;
  test("sum test") {
    assert(sumIntegers(1, 3) === 6)
  }

  test("generic summ test") {
    def inc(n: Double): Double = n + 1;
    def iden(n: Double): Double = n;

    assert(sumIntegers(1, 3) === summ(iden, 1, inc, 3))
  }

  test("iterative summ test") {
    def inc(n: Double): Double = n + 1;
    def iden(n: Double): Double = n;

    assert(sumIntegers(1, 3) === summIter(iden, 1, inc, 3))
  }

  import sicp1_3.Exercise1_31._;
  test("factorial test") {

    assert(24 === fact(4))
  }

  test("product test") {
    def inc(n: Double): Double = n + 1;
    def iden(n: Double): Double = n;

    assert(24 === product(iden, 1, inc, 4))
  }

  test("iterative product test") {
    def inc(n: Double): Double = n + 1;
    def iden(n: Double): Double = n;

    assert(24 === productIter(iden, 1, inc, 4))
  }
}