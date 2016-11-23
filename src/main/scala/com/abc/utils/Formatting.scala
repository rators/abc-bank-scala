package com.abc.utils

/**
  * Format utility functions
  */
object Formatting {

  /**
    * Formats a word. The word may be formatted to a plural or singular word.
    *
    * @param number
    * The number associated with this word. Will determine the plurality of the formatted result.
    * @param word
    * The word being formatted.
    * @return
    * The result of the formatting.
    */
  def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  /**
    * Converts a number to a dollar string.
    *
    * @param number
    * The number to be converted.
    * @return
    * The converted dollar string.
    */
  def toDollars(number: Double): String = f"$$$number%.2f"
}
