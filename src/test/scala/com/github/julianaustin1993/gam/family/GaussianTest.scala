package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import breeze.stats.distributions
import com.github.julianaustin1993.gam.link.{Identity, Inverse, getLink}
import org.scalatest.FunSuite

class GaussianTest extends FunSuite {
  val gaussian: Gaussian = Gaussian(getLink("Identity").get)
  val x: Double = scala.util.Random.nextFloat()
  val dist: distributions.Gaussian = breeze.stats.distributions.Gaussian(0, 1)
  val N = 200
  val y: DenseVector[Double] = DenseVector.rand(N, dist)
  val muHat: DenseVector[Double] = DenseVector.zeros[Double](N)
  val wts: DenseVector[Double] = DenseVector.ones[Double](size = N)
  val dev: DenseVector[Double] = gaussian.devResiduals(y, muHat, wts)

  test("testLink") {
    assert(gaussian.link == Identity())
    assert(Gaussian(getLink("Inverse").get).link == Inverse())
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
