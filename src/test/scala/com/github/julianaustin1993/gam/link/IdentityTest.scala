package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite

class IdentityTest extends FunSuite {
  val idLink: Identity = Identity()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(idLink.linkFun(x) - x) < eps)
  }
  test("inverse link function"){
    assert(math.abs(idLink.invLinkFun(x) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = idLink.invLinkFun compose idLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(idLink.muEta(x) - 1.0) < eps)
  }

  test("eta valid"){
    assert(idLink.validEta(x))
  }
}
