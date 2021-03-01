package com.github.julianaustin1993.gam.link

import com.github.julianaustin1993.gam.{EPS, INVEPS}

/**
 * Link object with the logit transformation.
 */
case class Logit() extends Link {
  val THRESH: Double = 30.0
  val MTHRESH: Double = -30.0

  override def linkFun: Double => Double = { mu: Double => math.log(mu / (1.0 - mu)) }

  override def invLinkFun: Double => Double = {
    case eta if eta < MTHRESH => EPS / (1.0 + EPS)
    case eta if eta < THRESH =>
      val eta_b = math.exp(eta)
      eta_b / (1.0 + eta_b)
    case eta => INVEPS / (1.0 + INVEPS)
  }

  override def muEta: Double => Double = {
    case eta if eta < MTHRESH => EPS
    case eta if eta < THRESH =>
      val opexp = 1.0 + math.exp(eta)
      math.exp(eta) / (opexp * opexp)
    case eta => EPS
  }

  override def validEta: Double => Boolean = { _: Double => true }
}
