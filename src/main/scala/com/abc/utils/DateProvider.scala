package com.abc.utils

import java.util.{Calendar, Date}

/**
  * The date provider companion object. Contains an instance of a Date Provider lazily (if needed only).
  */
object DateProvider {

  /**
    * The global date provider instance. Lazily evaluated.
    */
  lazy val instance: DateProvider = new DateProvider
}

/**
  * The date provider class.
  */
class DateProvider {

  /**
    * Convenience method for creating a calendar instance for the present.
    *
    * @return
    * A calendar instance dated now.
    */
  def now: Date = Calendar.getInstance.getTime

  val DAY_IN_MS = 1000l * 60l * 60l * 24l

  def tenDaysAgo = new Date(System.currentTimeMillis() - (10 * DAY_IN_MS))
}

