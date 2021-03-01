package com.github.julianaustin1993

import breeze.linalg.qr.DenseQR
import breeze.linalg.{*, DenseMatrix, DenseVector, qr, sum}
import breeze.numerics.{log, sqrt}
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import com.github.julianaustin1993.gam.family.Family

import scala.annotation.tailrec


package object gam {

  /**
   * Machine precision for double.
   */
  val EPS: Double = calculateMachineEpsilonDouble
  /**
   * Inverse of machine precision for double.
   */
  val INVEPS: Double = 1.0 / EPS

  /**
   * Function to calculate precision epsilon for scala double.
   *
   * @return The machine precision for scala double.
   */
  def calculateMachineEpsilonDouble: Double = {
    @tailrec
    def calc(machEps: Double): Double = {
      if ((1.0 + (machEps / 2.0)) != 1.0)
        calc(machEps / 2f)
      else
        machEps
    }

    calc(1.0)
  }

  /**
   * Wrapper function for y * log (y /mu) with 0 mapping to the limit.
   *
   * @param y  Dense vector of y values.
   * @param mu Dense vector of mu values.
   * @return Dense vector of y * log(y/mu) with 0 mapping to the limit.
   */
  def yLogYDivMu(y: DenseVector[Double], mu: DenseVector[Double]): DenseVector[Double] = {
    y *:* log((y /:/ mu).map(x => if (x == 0) 1 else x))
  }


  /**
   * Backsolve an upper-triangular linear system
   * with a single RHS
   *
   * @param A An upper-triangular matrix
   * @param y A single vector RHS
   * @return The solution, x, of the linear system A x = y
   */
  def backSolve(A: DenseMatrix[Double],
                y: DenseVector[Double]): DenseVector[Double] = {
    val yc = y.copy
    blas.dtrsv("U", "N", "N", A.cols, A.toArray,
      A.rows, yc.data, 1)
    yc
  }

  /**
   * Backsolve an upper-triangular linear system
   * with multiple RHSs
   *
   * @param A An upper-triangular matrix
   * @param Y A matrix with columns corresponding to RHSs
   * @return Matrix of solutions, X, to the linear system A X = Y
   */
  def backSolve(A: DenseMatrix[Double], Y: DenseMatrix[Double]): DenseMatrix[Double] = {
    val yc = Y.copy
    blas.dtrsm("L", "U", "N", "N", yc.rows, yc.cols, 1.0, A.toArray, A.rows, yc.data, yc.rows)
    yc
  }

  def wrapIrls(eta0: DenseVector[Double],
               y: DenseVector[Double],
               X: DenseMatrix[Double],
               weights: DenseVector[Double],
               offset: DenseVector[Double],
               family: Family,
               maxIts: Int,
               tol: Double = 1e-8): (DenseVector[Double], DenseMatrix[Double]) = {
    if (!eta0.map(family.link.validEta).reduce((x, y) => x & y)) {
      throw new Exception("Not valid starting values")
    }
    val mu0 = eta0.map(family.link.invLinkFun)
    if (!mu0.map(family.validMu).reduce((x, y) => x & y)) {
      throw new Exception("Not valid starting values")
    }
    val varMu = mu0.map(family.variance)
    val muEta = eta0.map(family.link.muEta)
    val z = (eta0 - offset) + (y - mu0) /:/ muEta
    val w = sqrt(weights *:* muEta *:* muEta /:/ varMu)
    val QR: DenseQR = qr.reduced(X(::, *) *:* w)
    val q: DenseMatrix[Double] = QR.q
    val r: DenseMatrix[Double] = QR.r
    val qty: DenseVector[Double] = q.t * (z *:* w)
    val coefs0 = backSolve(r, qty)
    val eta = offset + X * coefs0
    val mu = eta.map(family.link.invLinkFun)
    val dev0 = sum(family.devResiduals(y, mu, weights))
    val coefs = irls(coefs0, dev0, maxIts, y, X, weights, offset, family, maxIts, tol)
    val etaNew = offset + X * coefs
    val muNew = etaNew.map(family.link.invLinkFun)
    val varMuNew = muNew.map(family.variance)
    val muEtaNew = etaNew.map(family.link.muEta)
    val zNew = (etaNew - offset) + (y - muNew) /:/ muEtaNew
    val wNew = sqrt(weights *:* muEtaNew *:* muEtaNew /:/ varMuNew)
    val QRNew: DenseQR = qr.reduced(X(::, *) *:* w)
    (coefs, QR.r)
  }

  @annotation.tailrec
  def irls(
            coefs0: DenseVector[Double],
            dev0: Double,
            its: Int,
            y: DenseVector[Double],
            X: DenseMatrix[Double],
            weights: DenseVector[Double],
            offset: DenseVector[Double],
            family: Family,
            maxIts: Int,
            tol: Double = 1e-8): DenseVector[Double] = {
    val eta0 = offset + X * coefs0
    val mu0 = eta0.map(family.link.invLinkFun)
    val varMu = mu0.map(family.variance)
    val muEta = eta0.map(family.link.muEta)
    val z = (eta0 - offset) + (y - mu0) /:/ muEta
    val w = sqrt(weights *:* muEta *:* muEta /:/ varMu)
    val QR: DenseQR = qr.reduced(X(::, *) *:* w)
    val q: DenseMatrix[Double] = QR.q
    val r: DenseMatrix[Double] = QR.r
    val qty: DenseVector[Double] = q.t * (z *:* w)
    val (coefs, dev) = irlsAdjustStepSize(backSolve(r, qty), coefs0, y, X, weights, offset, family, maxIts)
    if (its <= 1) println("WARNING: IRLS did not converge")
    if (math.abs(dev - dev0) / (0.1 + math.abs(dev)) < tol | (its <= 1))
      coefs
    else
      irls(coefs, dev, its - 1, y, X, weights, offset, family, maxIts, tol)
  }

  def irlsAdjustStepSize(coefs: DenseVector[Double],
                         coefs0: DenseVector[Double],
                         y: DenseVector[Double],
                         X: DenseMatrix[Double],
                         weights: DenseVector[Double],
                         offset: DenseVector[Double],
                         family: Family,
                         maxIts: Int): (DenseVector[Double], Double) = {
    val eta = offset + X * coefs
    val mu = eta.map(family.link.invLinkFun)
    val dev = sum(family.devResiduals(y, mu, weights))
    val coefs1 = if (dev.isInfinite) irlsAdjustStepSizeDev(coefs, maxIts, coefs0, y, X, weights, offset, family) else coefs
    val eta1 = offset + X * coefs1
    val mu1 = eta1.map(family.link.invLinkFun)
    val check = (eta1.map(family.link.validEta) &:& mu1.map(family.validMu)).reduce(_ & _)
    val coefs2 = if (!check) irlsAdjustStepSizeDomain(coefs, maxIts, coefs0, y, X, weights, offset, family) else coefs1
    val eta2 = offset + X * coefs1
    val mu2 = eta2.map(family.link.invLinkFun)
    val dev2 = sum(family.devResiduals(y, mu2, weights))
    (coefs2, dev2)
  }


  @annotation.tailrec
  def irlsAdjustStepSizeDev(coefs: DenseVector[Double],
                            its: Int,
                            coefs0: DenseVector[Double],
                            y: DenseVector[Double],
                            X: DenseMatrix[Double],
                            weights: DenseVector[Double],
                            offset: DenseVector[Double],
                            family: Family): DenseVector[Double] = {
    val coefNew = 0.5 * (coefs + coefs0)
    val eta = offset + X * coefNew
    val mu = eta.map(family.link.invLinkFun)
    val dev = sum(family.devResiduals(y, mu, weights))
    if (its <= 1) println("WARNING: IRLS did not converge")
    if (!dev.isInfinite | its <= 1)
      coefNew
    else
      irlsAdjustStepSizeDev(coefNew, its - 1, coefs0, y, X, weights, offset, family)
  }

  @annotation.tailrec
  def irlsAdjustStepSizeDomain(coefs: DenseVector[Double],
                               its: Int,
                               coefs0: DenseVector[Double],
                               y: DenseVector[Double],
                               X: DenseMatrix[Double],
                               weights: DenseVector[Double],
                               offset: DenseVector[Double],
                               family: Family): DenseVector[Double] = {
    val coefNew = 0.5 * (coefs + coefs0)
    val eta = offset + X * coefNew
    val mu = eta.map(family.link.invLinkFun)
    val check = (eta.map(family.link.validEta) &:& mu.map(family.validMu)).reduce(_ & _)
    if (its <= 1) println("WARNING: IRLS did not converge")
    if (check | its <= 1)
      coefNew
    else
      irlsAdjustStepSizeDomain(coefNew, its - 1, coefs0, y, X, weights, offset, family)
  }
}
