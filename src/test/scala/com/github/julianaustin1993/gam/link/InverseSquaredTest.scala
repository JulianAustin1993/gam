package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite

class InverseSquaredTest extends FunSuite {
  val inverseSquaredLink: InverseSquared = InverseSquared()
  val x: Double = math.abs(scala.util.Random.nextFloat())
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(inverseSquaredLink.linkFun(x) - (1.0/(x*x))) < eps)
  }
  test("inverse link function"){
    assert(math.abs(inverseSquaredLink.invLinkFun(1.0/(x*x)) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = inverseSquaredLink.invLinkFun compose inverseSquaredLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(inverseSquaredLink.muEta(x) - (-1.0/(2*math.pow(x, 1.5)))) < eps)
  }

  test("eta valid"){
    assert(inverseSquaredLink.validEta(x))
    assert(!inverseSquaredLink.validEta(-1.0 * x))
  }
}
