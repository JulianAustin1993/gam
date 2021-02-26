package com.github.julianaustin1993

import scala.annotation.tailrec

package object gam {

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

  val EPS: Double = calculateMachineEpsilonDouble
  val INVEPS: Double = 1.0 / EPS

}
