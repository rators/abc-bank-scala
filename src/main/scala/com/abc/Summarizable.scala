package com.abc

/**
  * Trait for objects that can be summarized. Can be extended later on, or mixed in with other
  * traits that format in some way.
  */
trait Summarizable {
  def summarize: String
}
