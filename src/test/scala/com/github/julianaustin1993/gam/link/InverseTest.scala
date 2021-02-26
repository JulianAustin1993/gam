package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite

class InverseTest extends FunSuite {
  val inverseLink: Inverse = Inverse()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(inverseLink.linkFun(x) - (1.0/x)) < eps)
  }
  test("inverse link function"){
    assert(math.abs(inverseLink.invLinkFun(1.0/x) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = inverseLink.invLinkFun compose inverseLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(inverseLink.muEta(x) - (-1.0/(x*x))) < eps)
  }

  test("eta valid"){
    assert(inverseLink.validEta(x))
    assert(!inverseLink.validEta(0.0))
  }
}
