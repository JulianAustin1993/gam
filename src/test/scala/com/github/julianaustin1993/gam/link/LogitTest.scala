package com.github.julianaustin1993.gam.link

import breeze.stats.distributions.Gaussian
import org.scalatest.FunSuite

class LogitTest extends FunSuite {
  val logitLink: Logit = Logit()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8

  test("link function"){
    assert(math.abs(logitLink.linkFun(x) - math.log(x/(1.0 - x))) < eps)
  }
  test("inverse link function"){
    assert(math.abs(logitLink.invLinkFun(math.log(x/(1.0 - x))) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = logitLink.invLinkFun compose logitLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(logitLink.muEta(x) - math.exp(x)/((1+math.exp(x))*(1+math.exp(x)))) < eps)
  }

  test("eta valid"){
    assert(logitLink.validEta(x))
  }
}
