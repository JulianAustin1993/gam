package com.github.julianaustin1993.gam.glm

import breeze.linalg.qr.DenseQR
import breeze.linalg.{*, DenseMatrix, DenseVector, all, norm, qr}
import breeze.numerics.sqrt
import breeze.stats.distributions.{FDistribution, StudentsT}
import breeze.stats.mean
import com.github.julianaustin1993.gam.backSolve


/**
 * Weighted linear regression modelling.
 *
 * @param yVec      : Vector of responses.
 * @param xMat      : covariate matrix.
 * @param weightVec : Weight vector.
 * @param offsetVec : offset vector
 */
class Lm(yVec: DenseVector[Double], xMat: DenseMatrix[Double], weightVec: DenseVector[Double], offsetVec: DenseVector[Double]) extends Model {
  require(yVec.length == weightVec.length)
  require(yVec.length == xMat.rows)
  require(xMat.rows >= xMat.cols)
  require(all(weightVec >:> DenseVector.zeros[Double](weightVec.length)))

  /**
   * residuals
   */
  lazy val residuals: DenseVector[Double] = (y - (q * qty)) /:/ wts
  /**
   * fitted
   */
  lazy val fitted: DenseVector[Double] = yVec - residuals + offsetVec
  /**
   * weights
   */
  lazy val weights: DenseVector[Double] = weightVec
  /**
   * Dimension of problem including intercept and freedom.
   */
  lazy val nObservations: Int = X.rows
  lazy val nVariables: Int = X.cols
  lazy val df: Int = nObservations - nVariables
  /**
   * Residual sum of squares and residual squared error.
   */
  lazy val rss: Double = weights.t * (residuals *:* residuals)
  lazy val rse: Double = math.sqrt(rss / df)
  /**
   * Inverse of R matrix, and standard errors for the regression coefficients.
   */
  lazy val ri: DenseMatrix[Double] = backSolve(r, DenseMatrix.eye[Double](nVariables))
  lazy val se: DenseVector[Double] = norm(ri(*, ::)) * rse
  /**
   * T-statistic and p-values for regression coefficients.
   */
  lazy val t: DenseVector[Double] = coefficients / se
  lazy val p: DenseVector[Double] = t.map {
    1.0 - StudentsT(df).cdf(_)
  }.map {
    _ * 2.0
  }
  /**
   * Sum of squares of centres observations.
   */
  lazy val yVecBar: Double = mean(yVec)
  lazy val ssy: Double = (yVec - yVecBar).t * (yVec - yVecBar)
  /**
   * R^2 and adjusted R^2 for regression analysis.
   */
  lazy val rSquared: Double = (ssy - rss) / ssy
  lazy val adjRSquared: Double = 1.0 - ((nVariables - 1.0) / df) * (1.0 - rSquared)
  /**
   * F-statistic and p-values for regression analysis.
   */
  lazy val f: Double = (ssy - rss) / (nVariables - 1.0) / (rss / df)
  lazy val pf: Double = 1.0 - FDistribution(nVariables - 1, df).cdf(f)
  /**
   * Model matrix X weighted by wts.
   */
  override val X: DenseMatrix[Double] = xMat(::, *) *:* wts
  override val coefficients: DenseVector[Double] = backSolve(r, qty)
  val wts: DenseVector[Double] = sqrt(weightVec)
  /**
   * response vector y weighted by wts
   */

  val y: DenseVector[Double] = (yVec - offsetVec) *:* wts
  /**
   * QR decomposition of model matrix.
   */
  val QR: DenseQR = qr.reduced(X)
  val q: DenseMatrix[Double] = QR.q
  val r: DenseMatrix[Double] = QR.r
  /**
   * Fitting regression coefficients.
   */
  val qty: DenseVector[Double] = q.t * y

}

object Lm {
  /**
   * Constructor for Lm with only response, covariate and add intercept. Construct the design matrix.
   *
   * @param responseVec  Vector of responses
   * @param covariateMat Covariate matrix.
   * @param addIntercept Indicator as to whether to add intercept term to model.
   * @return Lm model
   */
  def apply(responseVec: DenseVector[Double], covariateMat: DenseMatrix[Double], addIntercept: Boolean): Lm = {
    val offset = DenseVector.zeros[Double](responseVec.length)
    val weight = DenseVector.ones[Double](responseVec.length)
    val designMat = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](covariateMat.rows).toDenseMatrix.t, covariateMat) else covariateMat
    new Lm(responseVec, designMat, weight, offset)
  }

  /**
   * Constructor for Lm with only response, covariate, weights and add intercept. Construct the design matrix.
   *
   * @param responseVec  Vector of responses
   * @param covariateMat Covariate matrix.
   * @param weightVec    Vector of weights for observations.
   * @param addIntercept Indicator as to whether to add intercept term to model.
   * @return Lm model
   */
  def apply(responseVec: DenseVector[Double], covariateMat: DenseMatrix[Double], weightVec: DenseVector[Double], addIntercept: Boolean): Lm = {
    val offset = DenseVector.zeros[Double](responseVec.length)
    val designMat = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](covariateMat.rows).toDenseMatrix.t, covariateMat) else covariateMat
    new Lm(responseVec, designMat, weightVec, offset)
  }

  /** *
   * Full constructor for Lm and construct the design matrix.
   *
   * @param responseVec  Vector of responses
   * @param covariateMat Covariate matrix.
   * @param weightVec    Vector of weights for observations.
   * @param offsetVec    Vector of offsets for responses.
   * @param addIntercept Indicator as to whether to add intercept term to model.
   * @return Lm model
   */
  def apply(responseVec: DenseVector[Double], covariateMat: DenseMatrix[Double], weightVec: DenseVector[Double], offsetVec: DenseVector[Double], addIntercept: Boolean): Lm = {
    val designMat = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](covariateMat.rows).toDenseMatrix.t, covariateMat) else covariateMat
    new Lm(responseVec, designMat, weightVec, offsetVec)
  }
}
