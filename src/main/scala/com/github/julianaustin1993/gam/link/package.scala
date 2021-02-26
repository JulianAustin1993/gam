package com.github.julianaustin1993.gam

package object link {
  def getLink(linkName: String): Option[Link] = {
    linkName match {
      case "Identity" => Some(Identity())
      case "Log" => Some(Log())
      case "Logit" => Some(Logit())
      case "Probit" => Some(Probit())
      case "Sqrt" => Some(Sqrt())
      case "Inverse" => Some(Inverse())
      case "InverseSquared" => Some(InverseSquared())
      case "CLogLog" => Some(CLogLog())
      case "Cauchit" => Some(Cauchit())
      case _ => None
    }
  }

}
