package com.github.julianaustin1993.gam.link

import com.github.julianaustin1993.gam.EPS


case class Log() extends Link {
  override def linkFun: Double => Double = { mu: Double => math.log(mu) }

  override def invLinkFun: Double => Double = { eta: Double => math.max(math.exp(eta), EPS) }

  override def muEta: Double => Double = { eta: Double => math.max(math.exp(eta), EPS) }

  override def validEta: Double => Boolean = { eta: Double => true}
}

