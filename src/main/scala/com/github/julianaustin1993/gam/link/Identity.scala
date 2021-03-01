package com.github.julianaustin1993.gam.link

/**
 * Link object with the identity transformation.
 */
case class Identity() extends Link {
  override def linkFun: Double => Double = { mu: Double => mu }
  override def invLinkFun: Double => Double = { eta: Double => eta }

  override def muEta: Double => Double = { eta: Double => 1.0 }

  override def validEta: Double => Boolean = { eta: Double => true }
}
