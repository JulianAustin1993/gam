package com.github.julianaustin1993.gam.glm

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite

class LmTest extends FunSuite {

  test("testApply") {
    val y = DenseVector(5.0, 5.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 4.0))
    val mod = new Lm(y, x, DenseVector.ones[Double](2), DenseVector.zeros[Double](2))
    assert(breeze.linalg.isClose(mod.weights, Lm(y, x, addIntercept = false).weights))
    assert(breeze.linalg.isClose(mod.weights, Lm(y, x, DenseVector.ones[Double](2), addIntercept = false).weights))
  }

  test("Handle 3 points on a slope, auto intercept") {
    val y = DenseVector(4.0, 5.0, 6.0)
    val x = DenseMatrix(2.0, 3.0, 4.0)
    val mod = Lm(y, x, addIntercept = true)
    val beta = DenseVector(2.0, 1.0)
    println(mod.coefficients)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
    assert(math.abs(mod.rSquared - 1.0) < 0.00001)
  }

  test("Handle 3 points on a slope, manual intercept") {
    val y = DenseVector(4.0, 5.0, 6.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0))
    val mod = Lm(y, x, addIntercept = false)
    val beta = DenseVector(2.0, 1.0)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
    assert(math.abs(mod.rSquared - 1.0) < 0.00001)
  }

  test("Handle 3 points on a slope, offset and manual intercept") {
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0))
    val offset = DenseVector.ones[Double](3)
    val beta = DenseVector(2.0, 1.0)
    val y = x * beta + offset
    val mod = Lm(y, x, DenseVector.ones[Double](3), offset, addIntercept = false)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
    assert(math.abs(mod.rSquared - 1.0) < 0.00001)
  }

  test("Handle 3 points on a slope, offset and auto intercept") {
    val x_no = DenseMatrix(2.0, 3.0, 4.0)
    val x = DenseMatrix((1.0, 2.0), (1.0, 3.0), (1.0, 4.0))
    val offset = DenseVector.ones[Double](3)
    val beta = DenseVector(2.0, 1.0)
    val y = x * beta + offset
    val mod = Lm(y, x_no, DenseVector.ones[Double](3), offset, addIntercept = true)
    assert(breeze.linalg.isClose(mod.coefficients, beta))
    assert(math.abs(mod.rSquared - 1.0) < 0.00001)
  }

}
