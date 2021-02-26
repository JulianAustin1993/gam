package com.github.julianaustin1993.gam.family

import breeze.linalg.DenseVector
import com.github.julianaustin1993.gam.link.Link

trait Family{
  val name: String
  val link: Link
  def variance: Double => Double
  def devResiduals: (DenseVector[Double], DenseVector[Double], DenseVector[Double]) => DenseVector[Double]
  def aic: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) => Double
  def validMu: Double => Boolean
}
