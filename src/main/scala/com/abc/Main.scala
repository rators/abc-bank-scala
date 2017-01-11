package com.abc

import java.util.Locale

import com.abc.utils.DateProvider
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
/**
  * The testing area for features.
  */
object Main extends App {
  val d = new DateTime()

  val formatter = DateTimeFormat.forPattern("dd/MM/yyyy")
  val dt = formatter.parseDateTime("10/12/2916")

  val range = DateProvider.dayRange(dt, DateProvider.now)
  println(s"${d.monthOfYear().getAsText(Locale.US)}")
}
