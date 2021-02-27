package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import breeze.stats.distributions
import com.github.julianaustin1993.gam.link.{Log, getLink}
import com.github.julianaustin1993.gam.logYDivMu
import org.scalatest.FunSuite

class PoissonTest extends FunSuite {
  val poisson: Poisson = Poisson(getLink("Log").get)
  val x: Double = math.abs(scala.util.Random.nextInt())
  val dist: distributions.Poisson = breeze.stats.distributions.Poisson(1)
  val N = 200
  val y: DenseVector[Double] = DenseVector.rand(N, dist).mapValues(_.toDouble)
  val muHat: DenseVector[Double] = DenseVector.ones[Double](N)
  val wts: DenseVector[Double] = DenseVector.ones[Double](size = N)
  val dev: DenseVector[Double] = poisson.devResiduals(y, muHat, wts)

  test("testVariance") {
    assert(poisson.variance(x) == x)
  }

  test("testLink") {
    assert(poisson.link == Log())
  }

  test("testName") {
    assert(poisson.name == "Poisson")
  }

  test("testDevResiduals") {
    val dev_calc = 2.0 * wts *:* (y *:* logYDivMu(y, muHat)) - 2.0 * wts *:* (y - muHat)
    assert(breeze.linalg.isClose(dev, dev_calc))
  }

  test("testAic") {
    val aic = poisson.aic(y, wts, muHat, wts, dev)
    val aic_off = poisson.aic(y, wts, 3.0*muHat, wts, dev)
    assert(aic < aic_off)
  }

}
