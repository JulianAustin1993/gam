package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import breeze.stats.distributions
import com.github.julianaustin1993.gam.link.{Identity, InverseSquared, getLink}
import org.scalatest.FunSuite

class WaldTest extends FunSuite {
  val wald: Wald = Wald(getLink("InverseSquared").get)
  val x: Double = math.abs(scala.util.Random.nextFloat())
  val dist: distributions.Wald = breeze.stats.distributions.Wald(1.0, 0.2)
  val N = 200
  val y: DenseVector[Double] = DenseVector.rand(N, dist)
  val muHat: DenseVector[Double] = DenseVector.ones[Double](N)
  val wts: DenseVector[Double] = DenseVector.ones[Double](size = N)
  val dev: DenseVector[Double] = wald.devResiduals(y, muHat, wts)
  test("testVariance") {
    assert(wald.variance(x) == math.pow(x, 3.0))
  }

  test("testLink") {
    assert(wald.link == InverseSquared())
    assert(Wald(getLink("Identity").get).link == Identity())
  }

  test("testValidMu") {
    assert(wald.validMu(x))
  }

  test("testName") {
    assert(wald.name == "Wald")
  }

  test("testDevResiduals") {
    val dev_calc = wts *:* ((y - muHat) *:* (y - muHat)) /:/ (y *:* muHat *:* muHat)
    assert(breeze.linalg.isClose(dev, dev_calc))
  }

  test("testAic") {
    val aic = wald.aic(y, wts, muHat, wts, dev)
    val dev_wrong = wald.devResiduals(y, N.toDouble * muHat, wts)
    val aic_off = wald.aic(y, wts, N.toDouble * muHat, wts, dev_wrong)
    assert(aic < aic_off)
  }
}
