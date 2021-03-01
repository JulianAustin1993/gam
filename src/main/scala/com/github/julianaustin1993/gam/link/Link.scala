package com.github.julianaustin1993.gam.link

/**
 * Link trait defining a Link for use in GLM.
 */
trait Link {

  /**
   * The link function to use in the GLM.
   *
   * @return link function evaluated at point.
   */
  def linkFun: Double => Double

  /**
   * The inverse of the link function
   *
   * @return Inverse of the link function evaluated at point.
   */
  def invLinkFun: Double => Double

  /**
   * The derivative of the inverse of the link function with respect to eta.
   *
   * @return Derivative evaluated at point.
   */
  def muEta: Double => Double

  /**
   * Function to checking validity of point in domain of eta.
   *
   * @return True if point is in the domain of eta, false otherwise.
   */
  def validEta: Double => Boolean
}

