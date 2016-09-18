package com.ferhtaydn.timeseriesexe

case class Series(time: Long, value: Double) {
  def prettyPrint: String = s"$time\t$value"
}

case class WindowResult(observations: Int, rollingSum: Double, minValue: Double, maxValue: Double) {
  def prettyPrint: String = {

    def formatRollingSum: String = {
      rollingSum.toInt.toString.length match {
        case 1 ⇒ f"$rollingSum%2.5f"
        case 2 ⇒ f"$rollingSum%2.4f"
        case 3 ⇒ f"$rollingSum%2.3f"
        case _ ⇒ f"$rollingSum%2.2f"
      }
    }

    f"\t$observations\t$formatRollingSum\t$minValue\t$maxValue"
  }
}

object TimeSeries {

  def runWindow(series: Array[Series], windowSize: Int): Array[(Series, WindowResult)] = {
    series.zipWithIndex.map {
      case (item @ Series(time, value), i) ⇒
        val seriesInWindow = series.take(i + 1).dropWhile(fs ⇒ fs.time <= time - windowSize)
        (item, evalWindowResult(seriesInWindow))
    }
  }

  def evalWindowResult(seriesInWindow: Array[Series]): WindowResult = {
    if (seriesInWindow.isEmpty) {
      WindowResult(0, 0d, 0d, 0d)
    } else {
      val s = seriesInWindow.map(_.value).sorted
      WindowResult(s.length, s.sum, s.head, s.last)
    }
  }

  def printResults(results: Array[(Series, WindowResult)], outputFile: String): Unit = {
    remove(outputFile)
    append(outputFile, s"T\t\tV\tN\tRS\tMinV\tMaxV")
    results.foreach {
      case (seriesItem, windowResult) ⇒
        append(outputFile, s"${seriesItem.prettyPrint}${windowResult.prettyPrint}")
    }
  }

}
