package com.ferhtaydn.timeseriesexe

object Main extends App {

  import TimeSeries._

  val timeWindow = 60

  val (inputFile: String, outputFile: String) = {
    if (args.length != 2) {
      Console.println("Please provide both the input and output files!")
      System.exit(0)
    } else {
      val inputFile: String = args(0)
      val outputFile: String = args(1)
      (inputFile, outputFile)
    }
  }

  def generateSeries(file: String): Option[Array[Series]] = {
    read(file).map(_.map(StringParser[Series]).flatMap(_.map(identity)))
  }

  generateSeries(inputFile) match {
    case Some(vs) ⇒
      if (vs.isEmpty) {
        Console.println("The file does not contain any timeseries!")
      } else {
        printResults(runWindow(vs, timeWindow), outputFile)
      }
    case None ⇒ Console.println(s"$inputFile file not found!")
  }

}
