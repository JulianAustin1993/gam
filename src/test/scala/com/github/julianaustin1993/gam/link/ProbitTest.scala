package com.github.julianaustin1993.gam.link

import breeze.stats.distributions.Gaussian
import org.scalatest.FunSuite

class ProbitTest extends FunSuite {
  val probitLink: Probit = Probit()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8
  val dist: Gaussian = Gaussian(0,1)

  test("link function"){
    assert(math.abs(probitLink.linkFun(x) - dist.inverseCdf(x)) < eps)
  }
  test("inverse link function"){
    assert(math.abs(probitLink.invLinkFun(dist.inverseCdf(x)) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = probitLink.invLinkFun compose probitLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(probitLink.muEta(x) - dist.pdf(x)) < eps)
  }

  test("eta valid"){
    assert(probitLink.validEta(x))
  }

}
