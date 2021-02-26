package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite


class SqrtTest extends FunSuite {
  val sqrtLink: Sqrt = Sqrt()
  val x: Double = math.abs(scala.util.Random.nextFloat())
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(sqrtLink.linkFun(x) - math.sqrt(x)) < eps)
  }
  test("inverse link function"){
    assert(math.abs(sqrtLink.invLinkFun(math.sqrt(x)) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = sqrtLink.invLinkFun compose sqrtLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(sqrtLink.muEta(x) - 2*x) < eps)
  }

  test("eta valid"){
    assert(sqrtLink.validEta(x))
    assert(!sqrtLink.validEta(-1.0*x))
  }

}
