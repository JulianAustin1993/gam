package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite


class LogTest extends FunSuite {
  val logLink: Log = Log()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(logLink.linkFun(x) - math.log(x)) < eps)
  }
  test("inverse link function"){
    assert(math.abs(logLink.invLinkFun(math.log(x))-x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = logLink.invLinkFun compose logLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(logLink.muEta(x)-logLink.invLinkFun(x)) < eps)
  }

  test("eta valid"){
    assert(logLink.validEta(x))
  }

}

