package com.github.julianaustin1993.gam.link

import breeze.stats.distributions.CauchyDistribution
import org.scalatest.FunSuite

class CauchitTest extends FunSuite {
  val cauchitLink: Cauchit = Cauchit()
  val x: Double = scala.util.Random.nextFloat()
  val eps: Double = 1E-8
  val dist: CauchyDistribution = CauchyDistribution(0,1)

  test("link function"){
    assert(math.abs(cauchitLink.linkFun(x) - dist.inverseCdf(x)) < eps)
  }
  test("inverse link function"){
    assert(math.abs(cauchitLink.invLinkFun(dist.inverseCdf(x)) - x) < eps)
  }
  test("inverse link function is inverse of link function"){
    def i = cauchitLink.invLinkFun compose cauchitLink.linkFun
    assert(math.abs(i(x) - x) < eps)
  }

  test("gradient of mu with respect to eta for glm."){
    assert(math.abs(cauchitLink.muEta(x) - dist.pdf(x)) < eps)
  }

  test("eta valid"){
    assert(cauchitLink.validEta(x))
  }

}
