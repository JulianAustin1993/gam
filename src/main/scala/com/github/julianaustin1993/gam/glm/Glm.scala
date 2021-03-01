package com.github.julianaustin1993.gam.glm

import breeze.linalg.{*, DenseMatrix, DenseVector, norm}
import breeze.stats.distributions.{FDistribution, StudentsT}
import breeze.stats.mean
import com.github.julianaustin1993.gam.family.Family
import com.github.julianaustin1993.gam.{backSolve, wrapIrls}

/**
 * Generalised Linear regression through Iteratively Re-weighted Least Squares.
 *
 * @param yVec      vector of responses.
 * @param xMat      Covariate matrix
 * @param weightVec vector of weights
 * @param offsetVec vector of offsets
 * @param expFamily exponential family instance.
 * @param muStart   vector of initial mu values
 * @param maxIts    Integer of maximum iterations.
 * @param tol       tolerance for convergence of the IRLS algorithm.
 */
class Glm(yVec: DenseVector[Double],
          xMat: DenseMatrix[Double],
          weightVec: DenseVector[Double],
          offsetVec: DenseVector[Double],
          expFamily: Family,
          muStart: DenseVector[Double],
          maxIts: Int,
          tol: Double) extends Model {

  /**
   * fitted
   */
  lazy val fitted: DenseVector[Double] = mu
  /**
   * residuals
   */
  lazy val residuals: DenseVector[Double] = y - fitted
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
  override val X: DenseMatrix[Double] = xMat
  val wts: DenseVector[Double] = weightVec
  val offset: DenseVector[Double] = offsetVec
  val family: Family = expFamily
  val y: DenseVector[Double] = yVec
  /**
   * Coefficient determined by IRLS algorithm
   */
  val (coefficients, r): (DenseVector[Double], DenseMatrix[Double]) = wrapIrls(muStart.map(family.link.linkFun), y, X, wts, offset, family, maxIts, tol)
  val eta: DenseVector[Double] = offset + X * coefficients
  val mu: DenseVector[Double] = eta.map(family.link.invLinkFun)

  def predictLinearPredictorSe(nX: DenseMatrix[Double]): DenseVector[Double] = {
    val rtix = nX * ri
    norm(rtix(*, ::))
  }
}

object Glm {
  /**
   * Constructor for Glm with no offset equal weights and defaults starting values.
   *
   * @param responseVec  vector of responses.
   * @param covariateMat Covariate matrix
   * @param expFamily    exponential family instance.
   * @param addIntercept Boolean to add intercept to model.
   * @return Glm
   */
  def apply(responseVec: DenseVector[Double], covariateMat: DenseMatrix[Double], expFamily: Family, addIntercept: Boolean): Glm = {
    val offset = DenseVector.zeros[Double](responseVec.length)
    val weight = DenseVector.ones[Double](responseVec.length)
    val designMat = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](covariateMat.rows).toDenseMatrix.t, covariateMat) else covariateMat
    val mu0 = expFamily.initialiseMu(responseVec, weight)
    new Glm(responseVec, designMat, weight, offset, expFamily, mu0, 1000, 1e-8)
  }

  def predictWithSe(newCovariates: DenseMatrix[Double], glm: Glm, response: Boolean, addIntercept: Boolean): (DenseVector[Double], DenseVector[Double]) = {
    val nX = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](newCovariates.rows).toDenseMatrix.t, newCovariates) else newCovariates
    val lp = nX * glm.coefficients
    val rtix = nX * glm.ri
    val selp = norm(rtix(*, ::))
    val fitted = if (response) lp.map(glm.family.link.invLinkFun) else lp
    val se = if (response) {
      selp *:* lp.map(glm.family.link.invLinkFun)
    } else selp
    (fitted, se)
  }

  def predict(newCovariates: DenseMatrix[Double], glm: Glm, response: Boolean, addIntercept: Boolean): DenseVector[Double] = {
    val nX = if (addIntercept) DenseMatrix.horzcat(DenseVector.ones[Double](newCovariates.rows).toDenseMatrix.t, newCovariates) else newCovariates
    val lp = nX * glm.coefficients
    if (response) lp.map(glm.family.link.invLinkFun) else lp
  }
}