package com.github.julianaustin1993.gam.link

import com.github.julianaustin1993.gam.EPS

/**
 * Link object with the complementary log-log transformation.
 */
case class CLogLog() extends Link{
  override def linkFun: Double => Double = { mu: Double => math.log(-1.0*math.log(1.0 - mu)) }

  override def invLinkFun: Double => Double = { eta: Double => math.max(math.min(-math.expm1( -1.0 * math.exp(eta)), 1.0 - EPS ), EPS) }

  override def muEta: Double => Double = { eta: Double =>{
    val eta_b = math.min(eta, 700.0)
    math.max(math.exp(eta_b) * math.exp(-1.0 * math.exp(eta_b)), EPS)
  }}

  override def validEta: Double => Boolean = { eta: Double => true }
}
