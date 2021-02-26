package com.github.julianaustin1993.gam.link

trait Link {

  def linkFun: Double => Double

  def invLinkFun: Double => Double

  def muEta: Double => Double

  def validEta: Double => Boolean
}

