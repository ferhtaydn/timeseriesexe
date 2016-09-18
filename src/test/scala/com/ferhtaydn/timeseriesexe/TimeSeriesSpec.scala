package com.ferhtaydn.timeseriesexe

import org.scalatest.{ Matchers, WordSpec }

class TimeSeriesSpec extends WordSpec with Matchers {

  "testEvalWindowResult" when {

    "seriesInWindow is empty" should {
      "return WindowResult with zeros" in {
        val result = TimeSeries.evalWindowResult(Array.empty[Series])
        result shouldBe WindowResult(0, 0d, 0d, 0d)
      }
    }

    "seriesInWindow has one item" should {
      "return WindowResult with that item info" in {
        val result = TimeSeries.evalWindowResult(Array(Series(1L, 1d)))
        result shouldBe WindowResult(1, 1d, 1d, 1d)
      }
    }

    "seriesInWindow has more than one item" should {
      "return WindowResult with correct calculation" in {
        val result = TimeSeries.evalWindowResult(Array(Series(1L, 1d), Series(2L, 2d)))
        result shouldBe WindowResult(2, 3d, 1d, 2d)
      }
    }

  }

  "testRunWindow" when {

    "series is empty" should {
      "return empty array" in {
        val result = TimeSeries.runWindow(Array.empty[Series], 60)
        result shouldBe Array.empty[(Series, WindowResult)]
      }
    }

    "window size is 0" should {
      "return all the arrays with empty window results" in {
        val result = TimeSeries.runWindow(Array(Series(1L, 1d), Series(2L, 2d)), 0)
        result shouldBe Array(
          (Series(1L, 1d), WindowResult(0, 0d, 0d, 0d)),
          (Series(2L, 2d), WindowResult(0, 0d, 0d, 0d))
        )
      }
    }

    "window size is 1" should {
      "return all the arrays with their only window results" in {
        val result = TimeSeries.runWindow(Array(Series(1L, 1d), Series(2L, 2d)), 1)
        result shouldBe Array(
          (Series(1L, 1d), WindowResult(1, 1d, 1d, 1d)),
          (Series(2L, 2d), WindowResult(1, 2d, 2d, 2d))
        )
      }
    }

    "window size is 60" should {
      "return all the arrays with processed window results" in {
        val result = TimeSeries.runWindow(Array(Series(1L, 1d), Series(2L, 2d)), 60)
        result shouldBe Array(
          (Series(1L, 1d), WindowResult(1, 1d, 1d, 1d)),
          (Series(2L, 2d), WindowResult(2, 3d, 1d, 2d))
        )
      }
    }

  }
}
