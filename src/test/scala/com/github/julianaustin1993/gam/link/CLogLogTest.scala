package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite

class CLogLogTest extends FunSuite {
  val cloglogLink: CLogLog = CLogLog()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(cloglogLink.linkFun(x) - math.log( -1.0 * math.log( 1.0 - x))) < eps)
  }
  test("inverse link function"){
    assert(math.abs(cloglogLink.invLinkFun(math.log( -1.0 * math.log( 1.0 - x))) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = cloglogLink.invLinkFun compose cloglogLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(cloglogLink.muEta(x) - math.exp(x) * math.exp(-1.0*math.exp(x))) < eps)
  }

  test("eta valid"){
    assert(cloglogLink.validEta(x))
  }
}
