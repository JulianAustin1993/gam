package com.github.julianaustin1993.gam.link

import breeze.stats.distributions.CauchyDistribution
import com.github.julianaustin1993.gam.EPS

case class Cauchit() extends Link {
  val dist: CauchyDistribution = CauchyDistribution(0, 1)

  override def linkFun: Double => Double = { mu: Double => dist.inverseCdf(mu) }

  override def invLinkFun: Double => Double = { eta: Double => {
    val thresh = -1.0 * dist.inverseCdf(EPS)
    val eta_b = math.min(math.max(eta, -1.0 * thresh), thresh)
    dist.cdf(eta_b)
  }
  }

  override def muEta: Double => Double = { eta: Double => math.max(dist.pdf(eta), EPS) }

  override def validEta: Double => Boolean = { eta: Double => true }
}
