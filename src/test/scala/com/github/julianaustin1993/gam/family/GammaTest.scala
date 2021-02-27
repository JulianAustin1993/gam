package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import breeze.stats.distributions
import com.github.julianaustin1993.gam.link.{Identity, Inverse, getLink}
import com.github.julianaustin1993.gam.logYDivMu
import org.scalatest.FunSuite

class GammaTest extends FunSuite {
  val gamma: Gamma = Gamma(getLink("Inverse").get)
  val x: Float = math.abs(scala.util.Random.nextFloat())
  val dist: distributions.Gamma = breeze.stats.distributions.Gamma(1.0, 2.0)
  val N = 10
  val y: DenseVector[Double] = DenseVector.rand(N, dist)
  val muHat: DenseVector[Double] = DenseVector.ones[Double](N) * 2.0
  val wts: DenseVector[Double] = DenseVector.ones[Double](size = N)
  val dev: DenseVector[Double] = gamma.devResiduals(y, muHat, wts)

  test("testLink") {
    assert(gamma.link == Inverse())
    assert(Gamma(getLink("Identity").get).link == Identity())
  }

  test("testValidMu") {
    assert(gamma.validMu(x))
    assert(!gamma.validMu(-1.0 * x))
  }

  test("testName") {
    assert(gamma.name == "Gamma")

  }

  test("testDevResiduals") {
    val dev_calc = -2.0 * wts *:* (logYDivMu(y, muHat) - ((y - muHat) /:/ muHat))
    assert(breeze.linalg.isClose(dev, dev_calc))
  }

  test("testAic") {
    val aic = gamma.aic(y, wts, muHat, wts, dev)
    val dev_wrong = gamma.devResiduals(y, 2.0 * muHat, wts)
    val aic_off = gamma.aic(y, wts, muHat, wts, dev_wrong)
    assert(aic < aic_off)
  }
}
