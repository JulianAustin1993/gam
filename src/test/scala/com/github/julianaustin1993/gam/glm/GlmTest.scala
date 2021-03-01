package com.github.julianaustin1993.gam.glm

import breeze.linalg.{DenseMatrix, DenseVector, isClose}
import com.github.julianaustin1993.gam.family.{Binomial, Gaussian, Poisson, Wald}
import org.scalatest.FunSuite

class GlmTest extends FunSuite {

  test("Simple apply") {
    val y = DenseVector(8.0, 11.0, 14.0)
    val x = DenseMatrix(2.0, 3.0, 4.0)
    val f = Gaussian()
    val mod = Glm(y, x, f, addIntercept = true)
    val beta = DenseVector(2.0, 3.0)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
  }

  test("Handle 3 points on a slope.") {
    val y = DenseVector(8.0, 11.0, 14.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0))
    val f = Gaussian()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = DenseVector.zeros[Double](y.length)
    val mod = new Glm(y, x, w, o, f, mu0, 25, 1e-8)
    val beta = DenseVector(2.0, 3.0)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
  }

  test("Gaussian Glm gives same as Lm") {
    val y = DenseVector(8.0, 11.0, 14.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0))
    val f = Gaussian()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = DenseVector.zeros[Double](y.length)
    val mod = new Glm(y, x, w, o, f, mu0, 25, 1e-8)
    val modLm = Lm(y, x, addIntercept = false)
    val beta = DenseVector(2.0, 3.0)
    assert(breeze.linalg.isClose(mod.coefficients, modLm.coefficients))
    assert(breeze.linalg.isClose(mod.coefficients, beta))
  }

  test("Handle simple logistic regression same as R.") {
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0), (1.0, 5.0), (1.0, 6.0))
    val y = DenseVector(1.0, 0.0, 1.0, 0.0, 1.0)
    val f = Binomial()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = DenseVector.ones[Double](y.length) * 0.5
    val mod = new Glm(y, x, w, o, f, mu0, 1000, 1e-8)
    // Coefficients same as in R.
    val R = org.ddahl.rscala.RClient()
    R.eval("y = %-", y.toArray)
    R.eval("x = %-", x(::, 1).toDenseVector.toArray)
    R.eval("mod = glm(y~x,family=binomial())")
    val rCoef = DenseVector[Double](R.evalD1("mod$coefficients"))
    assert(isClose(mod.coefficients, rCoef))
  }

  test("Handle simple poisson regression same as R.") {
    val y = DenseVector(1.0, 2.0, 3.0, 2.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 4.0), (1.0, 5.0), (1.0, 2.5))
    val f = Poisson()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = DenseVector.ones[Double](y.length) * 0.5
    val mod = new Glm(y, x, w, o, f, mu0, 1000, 1e-8)
    // Coefficients same as in R.
    val R = org.ddahl.rscala.RClient()
    R.eval("y = %-", y.toArray)
    R.eval("x = %-", x(::, 1).toDenseVector.toArray)
    R.eval("mod = glm(y~x,family=poisson())")
    val rCoef = DenseVector[Double](R.evalD1("mod$coefficients"))
    assert(isClose(mod.coefficients, rCoef))
  }

  test("Handle simple gaussian regression same as R.") {
    val y = DenseVector(1.0, 2.0, 3.0, 2.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 4.0), (1.0, 5.0), (1.0, 2.5))
    val f = Gaussian()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = DenseVector.ones[Double](y.length) * 0.5
    val mod = new Glm(y, x, w, o, f, mu0, 1000, 1e-8)
    // Coefficients same as in R.
    val R = org.ddahl.rscala.RClient()
    R.eval("y = %-", y.toArray)
    R.eval("x = %-", x(::, 1).toDenseVector.toArray)
    R.eval("mod = glm(y~x,family=gaussian())")
    val rCoef = DenseVector[Double](R.evalD1("mod$coefficients"))
    assert(isClose(mod.coefficients, rCoef))
  }

  test("Handle simple Wald regression same as R.") {
    val y = DenseVector(1.0, 2.0, 3.0, 2.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 4.0), (1.0, 5.0), (1.0, 2.5))
    val f = Wald()
    val w = DenseVector.ones[Double](y.length)
    val o = DenseVector.zeros[Double](y.length)
    val mu0 = y
    val mod = new Glm(y, x, w, o, f, mu0, 10, 1e-8)
    // Coefficients same as in R.
    val R = org.ddahl.rscala.RClient()
    R.eval("y = %-", y.toArray)
    R.eval("x = %-", x(::, 1).toDenseVector.toArray)
    R.eval("mod = glm(y~x,family=inverse.gaussian)")
    val rCoef = DenseVector[Double](R.evalD1("mod$coefficients"))
    assert(isClose(mod.coefficients, rCoef))
  }

}
