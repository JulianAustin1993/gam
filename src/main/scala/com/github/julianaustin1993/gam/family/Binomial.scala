package com.github.julianaustin1993.gam.family

import breeze.linalg.{DenseVector, any}
import com.github.julianaustin1993.gam.link.{Link, Logit}
import com.github.julianaustin1993.gam.yLogYDivMu
import spire.implicits.cfor

/**
 * Binomial family with link function provided.
 *
 * @param link link function to use in the glm of this family. Defaults to the canonical logit link.
 */
case class Binomial(link: Link = Logit()) extends Family {
  override val name: String = "Binomial"

  override def variance: Double => Double = mu => mu * (1 - mu)

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      val v1 = yLogYDivMu(y, mu)
      val v2 = yLogYDivMu(1.0 - y, 1.0 - mu)
      2.0 * wt *:* (v1 + v2)
    }
  }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, n, mu, wt, _) => {
      val m = if (any(n >:> 1.0)) n else wt
      val r = DenseVector.zeros[Double](y.length)
      cfor(0)(i => i < r.length, i => i + 1)(i => {
        r(i) = breeze.stats.distributions.Binomial(m(i).round.toInt, mu(i)).logProbabilityOf((y(i) * m(i)).round.toInt)
      })
      -2.0 * ((wt /:/ m).map(x => if (x.isInfinite) 0 else x).t * r)
    }
  }

  override def validMu: Double => Boolean = mu => mu > 0.0 && mu < 1.0

  override def initialiseMu: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, wts) => (wts *:* y + 0.5) /:/ (wts + 1.0)
  }
}
