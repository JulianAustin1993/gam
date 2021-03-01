package com.github.julianaustin1993.gam.link

/**
 * Link object with the square root transformation.
 */
case class Sqrt() extends Link{
  override def linkFun: Double => Double = { mu: Double => math.sqrt(mu) }

  override def invLinkFun: Double => Double = { eta: Double => eta * eta }

  override def muEta: Double => Double = { eta: Double => 2.0 * eta }

  override def validEta: Double => Boolean = { eta: Double => eta > 0 }
}
