package com.abc.utils

import java.util.{Calendar, Date}

import com.abc.customer.Account
import org.joda._
import org.joda.time._

/**
  * The date provider companion object. Contains an instance of a Date Provider lazily (if needed only).
  */
object DateProvider {

  def dayRange(start: DateTime, end: DateTime): Days = {
    Days.daysBetween(start, end)
  }

  def tenDaysAgo(date: DateTime) = date.minus(Days.days(10))

  def now = new DateTime()

  val DAY_IN_MS = 1000l * 60l * 60l * 24l
}

