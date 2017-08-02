package com.github.zenpie.macrowave.internal

private[internal] trait NameProvider {
  def fresh(name: String): String
}

private[internal] object NameProvider {

  private val SyntheticName = "(.*)\\$(?:[0-9]+)".r

  def counting(): NameProvider = new NameProvider {
    private var counters = Map.empty[String, Int]

    def fresh(baseName: String): String = baseName match {
      case SyntheticName(name) => freshName(name)
      case _                   => freshName(baseName)
    }

    private def freshName(baseName: String): String = {
      val counter = counters.getOrElse(baseName, 0)
      val mangledName = s"$baseName$$$counter"
      counters += (baseName -> (counter + 1))
      mangledName
    }
  }

}
