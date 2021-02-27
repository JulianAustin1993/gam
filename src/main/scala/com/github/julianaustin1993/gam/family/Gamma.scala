package com.github.julianaustin1993.gam.family

import breeze.linalg.{DenseVector, sum}
import com.github.julianaustin1993.gam.link.{Inverse, Link}
import com.github.julianaustin1993.gam.logYDivMu
import spire.implicits.cfor

case class Gamma(link: Link = Inverse()) extends Family {
  override val name: String = "Gamma"

  override def variance: Double => Double = mu => mu * mu

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      -2.0 * wt *:* (logYDivMu(y, mu) - ((y - mu) /:/ mu))
    }
  }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, _, mu, wt, devRes) => {
      val m = sum(wt)
      val disp = sum(devRes) / m
      val r = DenseVector.zeros[Double](y.length)
      cfor(0)(i => i < r.length, i => i + 1)(i => {
        r(i) = breeze.stats.distributions.Gamma(1.0 / disp, mu(i) * disp).logPdf(y(i))
      })
      -2.0 * (wt.t * r) + 2
    }
  }

  override def validMu: Double => Boolean = mu => mu > 0 & !mu.isInfinite
}
