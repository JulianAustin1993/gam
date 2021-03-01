package com.github.julianaustin1993.gam.family

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.github.julianaustin1993.gam.link.{InverseSquared, Link}

/**
 * Wald or inverse gaussian family with link function provided.
 *
 * @param link link function to use in the glm of this family. Defaults to the canonical inverse squared link.
 */
case class Wald(link: Link = InverseSquared()) extends Family {
  override val name: String = "Wald"

  override def variance: Double => Double = mu => mu * mu * mu

  override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      val yc = y - mu
      wt *:* (yc *:* yc) /:/ (y *:* mu *:* mu)
    }
  }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, _, _, wt, devRes) => {
      val sumWt = sum(wt)
      val dev = sum(devRes)
      sumWt * (1.0 + math.log(dev / sumWt) * 2.0 * math.Pi) + 3.0 * (log(y).t * wt) + 2.0
    }
  }

  override def validMu: Double => Boolean = _ => true

  override def initialiseMu: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, _) => y
  }
}
