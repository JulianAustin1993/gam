package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.{Link, Log}
import com.github.julianaustin1993.gam.yLogYDivMu
import spire.implicits.cfor

/**
 * Poisson family with link function provided.
 *
 * @param link link function to use in the glm of this family. Defaults to the canonical log link.
 */
case class Poisson(link: Link=Log()) extends Family{
  override val name: String = "Poisson"

  override def variance: Double => Double = { mu: Double => mu}

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      2.0 * wt *:* (yLogYDivMu(y, mu) - (y - mu))
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

  override def initialiseMu: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, _) => y + 0.1
  }
}
