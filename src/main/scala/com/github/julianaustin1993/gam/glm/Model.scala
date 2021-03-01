package com.github.julianaustin1993.gam.glm

import breeze.linalg.{DenseMatrix, DenseVector}

/**
 * Model trait.
 */
trait Model {
  /**
   * Model matrix X.
   */
  val X: DenseMatrix[Double]

  /**
   * Fitted coefficients for model.
   */
  val coefficients: DenseVector[Double]

}
