package com.github.julianaustin1993.gam.family

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import com.github.julianaustin1993.gam.link.{Identity, Link}

/**
 * Gaussian family with link function provided.
 *
 * @param link link function to use in the glm of this family. Defaults to the canonical identity link.
 */
case class Gaussian(link: Link = Identity()) extends Family {
  override val name: String = "Gaussian"

  override def variance: Double => Double = _ => 1.0

    override def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, mu, wt) => {
      val res = y - mu
      wt *:* res *:* res
    }
    }

  override def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double = {
    (y, _, _, wt, devRes) => {
      val nobs = y.length
      val dev = sum(devRes)
      val sumLogWt = sum(log(wt))
      nobs * (math.log(dev / nobs * 2 * math.Pi) + 1) + 2 - sumLogWt
    }
  }

  override def validMu: Double => Boolean = _ => true

  override def initialiseMu: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double] = {
    (y, _) => y
  }
}
