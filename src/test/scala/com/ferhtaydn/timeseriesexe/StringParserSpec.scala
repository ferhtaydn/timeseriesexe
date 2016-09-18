package com.ferhtaydn.timeseriesexe

import org.scalatest.{ Matchers, WordSpec }

class StringParserSpec extends WordSpec with Matchers {

  import StringParser._

  "testDoubleParser" when {

    "string is not in double format" should {
      "return None" in {
        val result = StringParser[Double]("123a.29")
        result shouldBe None
      }
    }

    "string is in double format" should {
      "return double value in Some" in {
        val result = StringParser[Double]("123.29")
        result shouldBe Some(123.29)
      }
    }
  }

  "testLongParser" when {

    "string is not in long format" should {
      "return None" in {
        val result = StringParser[Long]("123aL")
        result shouldBe None
      }
    }

    "string is in long format" should {
      "return long value Some" in {
        val result = StringParser[Long]("123")
        result shouldBe Some(123L)
      }
    }
  }

  "testSeriesParser" when {

    "time string is not in long format" should {
      "return None" in {
        val result = StringParser[Series]("123aL\t123")
        result shouldBe None
      }
    }

    "value string is not in double format" should {
      "return None" in {
        val result = StringParser[Series]("123L\t123a.23")
        result shouldBe None
      }
    }

    "string is not in (long double) format" should {
      "return None" in {
        val result = StringParser[Series]("123L\\s123.23")
        result shouldBe None
      }
    }

    "string is not in (long double) format 2" should {
      "return None" in {
        val result = StringParser[Series]("123L")
        result shouldBe None
      }
    }

    "string is in (long double) format" should {
      "return correct series" in {
        val result = StringParser[Series]("135\t1.80")
        result shouldBe Some(Series(135, 1.80))
      }
    }

  }

}
