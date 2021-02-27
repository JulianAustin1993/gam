package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.{Link, Log}
import com.github.julianaustin1993.gam.logYDivMu
import spire.implicits.cfor


case class Poisson(link: Link=Log()) extends Family{
  override val name: String = "Poisson"

  override def variance: Double => Double = { mu: Double => mu}

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      2.0 * wt *:* (y *:* logYDivMu(y, mu) - (y - mu))
    }
  }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, _, mu, wt, _) => {
      val r = DenseVector.zeros[Double](y.length)
      cfor(0)(i => i < r.length, i => i + 1)(i => {
        r(i) = breeze.stats.distributions.Poisson(mu(i)).logProbabilityOf(y(i).toInt)
      })
      -2.0 * (r.t * wt)
    }
  }

  override def validMu: Double => Boolean = _ > 0
}
