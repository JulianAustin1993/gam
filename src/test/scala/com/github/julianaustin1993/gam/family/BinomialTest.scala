package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.{Logit, getLink}
import org.scalatest.FunSuite

class BinomialTest extends FunSuite {
  val binomial = Binomial(getLink("Logit").get)
  val x: Int = if (scala.util.Random.nextBoolean()) 1 else 0
  val mu: Double = scala.util.Random.between(0.0, 1.0)
  val cases: Int = 10
  val dist = breeze.stats.distributions.Binomial(10, mu)
  val N = 20
  val y = DenseVector.rand(N, dist).mapValues(_.toDouble/cases.toDouble)
  val muHat = DenseVector.ones[Double](N) * mu
  val wts = DenseVector.ones[Double](size=N)* cases.toDouble
  val dev = binomial.devResiduals(y, muHat, wts)
  test("testVariance") {
    assert(binomial.variance(mu) == mu * (1-mu))
  }

  test("testLink") {
    assert(binomial.link == Logit())
  }

  test("testValidMu") {
    assert(binomial.validMu(mu))
    assert(!binomial.validMu(-1.0*mu))
    assert(!binomial.validMu(1.0 + mu))
  }

  test("testDevResiduals") {
    val v1 = y *:* (y /:/ muHat).map(x => if (x == 0) 0 else math.log(x))
    val v2 = (1.0 - y) *:*((1.0 - y) /:/ (1.0 - muHat)).map(x => if (x == 0) 0 else math.log(x))
    assert(breeze.linalg.isClose(dev, 2.0 * wts *:* (v1 + v2)))
  }

  test("testAic") {
    val aic = binomial.aic(y, wts, muHat, wts, dev)
    val aic_off = binomial.aic(y, wts, if(mu>0.3) muHat - 0.2 else muHat + 0.2, wts, dev)
    assert(aic < aic_off)
  }

}