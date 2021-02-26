package com.github.julianaustin1993.gam.link

import org.scalatest.FunSuite

class packageTest extends FunSuite {

  test("testGetLink") {
    val link = getLink("Cauchit").get
    assert(link == Cauchit())

    val badLink = getLink("Blag").getOrElse(1)
    assert(badLink==1)
  }

}
