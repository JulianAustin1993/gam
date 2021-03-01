package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.Link

/**
 * Family trait defining a exponential family for use in GLM.
 */
trait Family {
  /**
   * Name of the family.
   */
  val name: String
  /**
   * Link function to use with the family.
   */
  val link: Link

  /**
   * Variance as a function of the mean of the family.
   *
   * @return variance at point.
   */
  def variance: Double => Double

  /**
   * Function to compute deviance of observations from the mean with weighting.
   *
   * @return The vector of deviance residuals from mean points with weighting.
   */
  def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double]

  /**
   * Function to compute the Akaike information criterion (AIC) for the family.
   *
   * @return AIC of the family at observations.
   */
  def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double

  /**
   * Function to check to see if a point lies in the domain of the mean function.
   *
   * @return True if point is in the domain of the mean function, false otherwise.
   */
  def validMu: Double => Boolean

  /**
   * Initialise starting values of the mu for Glm
   *
   * @return Initial starting values for mu.
   */
  def initialiseMu: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double]

}
