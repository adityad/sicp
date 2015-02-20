package sicp1_3

object Exercise1_35 {

  type lambda = BigDecimal => BigDecimal

  def fixedPointGoldenRatio(f: lambda): BigDecimal = {
    def fixedPointGoldenRatioHelper(guess: BigDecimal): BigDecimal = {
      if ((f(guess) - guess).abs < 0.0001) guess
      else fixedPointGoldenRatioHelper(f(guess))
    }

    fixedPointGoldenRatioHelper(1.0)
  }
}