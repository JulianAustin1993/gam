package com.github.julianaustin1993.gam.link

/**
 * Link object with the inverse transformation.
 */
case class Inverse() extends Link{
  override def linkFun: Double => Double = { mu: Double => 1.0 / mu }

  override def invLinkFun: Double => Double = { eta: Double => 1.0 / eta }

  override def muEta: Double => Double = { eta: Double => -1.0 / (eta * eta) }

  override def validEta: Double => Boolean = { eta: Double => eta != 0 }
}
