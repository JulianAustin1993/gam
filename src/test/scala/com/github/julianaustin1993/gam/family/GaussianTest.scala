package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.{Identity, Inverse, getLink}
import org.scalatest.FunSuite

class GaussianTest extends FunSuite {
  val gaussian = Gaussian(getLink("Inverse").get)
  val x: Double = scala.util.Random.nextFloat()
  val dist = breeze.stats.distributions.Gaussian(0, 1)
  val N = 200
  val y = DenseVector.rand(N, dist)
  val muHat = DenseVector.zeros[Double](N)
  val wts = DenseVector.ones[Double](size=N)
  val dev = gaussian.devResiduals(y, muHat, wts)

  test("testLink") {
    assert(gaussian.link == Inverse())
    assert(Gaussian().link == Identity())
  }

  test("testValidMu") {
    assert(gaussian.validMu(x))
  }

  test("testName") {
    assert(gaussian.name == "Gaussian")

  }

  test("testDevResiduals") {
    val dev_calc = wts * (y - muHat) * (y-muHat)
    assert(breeze.linalg.isClose(dev, dev_calc))
  }

  test("testAic") {
    val aic = gaussian.aic(y, wts, muHat, wts, dev)
    val dev_wrong = gaussian.devResiduals(y, N.toDouble*wts, wts)
    val aic_off = gaussian.aic(y, wts, muHat, wts, dev_wrong)
    assert(aic < aic_off)
  }

}
