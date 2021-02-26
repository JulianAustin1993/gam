package com.github.julianaustin1993.gam.family

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.constants.E
import breeze.numerics.log
import com.github.julianaustin1993.gam.link.{Link, Log}
import spire.implicits.cfor


case class Poisson(link: Link=Log()) extends Family{
  override val name: String = "Poisson"

  override def variance: Double => Double = { mu: Double => mu}

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      2.0 * wt *:* (y *:* log((y /:/ mu).map(x => if (x == 0) E else x)) - (y - mu))
    }
  }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, n, mu, wt, dev) => {
      val r = DenseVector.zeros[Double](y.length)
      cfor(0)( i => i < r.length, i => i+1)( i => {
        r(i) = breeze.stats.distributions.Poisson(mu(i)).logProbabilityOf(y(i).toInt)
      })
      -2.0 * sum(r *:* wt)
    }
  }

  override def validMu: Double => Boolean = _ > 0
}
