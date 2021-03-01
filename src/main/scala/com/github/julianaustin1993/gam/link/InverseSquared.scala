package com.github.julianaustin1993.gam.link

/**
 * Link object with the inverse squared transformation.
 */
case class InverseSquared() extends Link {
  override def linkFun: Double => Double = { mu: Double => 1.0 / (mu * mu) }

  override def invLinkFun: Double => Double = { eta: Double => 1.0/math.sqrt(eta) }

  override def muEta: Double => Double = { eta: Double => -1.0 / (2 *math.pow(eta, 1.50)) }

  override def validEta: Double => Boolean = { eta: Double => eta > 0}
}
