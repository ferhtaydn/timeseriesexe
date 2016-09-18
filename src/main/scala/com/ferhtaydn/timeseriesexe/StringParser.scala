package com.ferhtaydn.timeseriesexe

import scala.util.Try

trait StringParser[A] {
  def apply(s: String): Option[A]
}

object StringParser {

  def apply[A](s: String)(implicit parser: StringParser[A]): Option[A] = parser(s)

  implicit val seriesParser: StringParser[Series] = new StringParser[Series] {
    override def apply(s: String): Option[Series] = s.split("\t").toList match {
      case List(t, v) ⇒
        longParser(t) match {
          case Some(time) ⇒ doubleParser(v) match {
            case Some(value) ⇒ Some(Series(time, value))
            case None        ⇒ None
          }
          case None ⇒ None
        }
      case _ ⇒ None
    }
  }

  implicit val longParser: StringParser[Long] = new StringParser[Long] {
    override def apply(s: String): Option[Long] = Try(s.toLong).toOption
  }

  implicit val doubleParser: StringParser[Double] = new StringParser[Double] {
    override def apply(s: String): Option[Double] = Try(s.toDouble).toOption
  }
}
