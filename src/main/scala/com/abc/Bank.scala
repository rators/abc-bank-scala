package com.abc

import com.abc.customer.Customer

/**
  * The case class representation for a bank.
  *
  * @param customers
  * The customers associated with this bank.
  */
case class Bank(customers: List[Customer]) extends Summarizable {
  val SUMMARY_TITLE: String = "Customer Summary"

  /**
    * The summary associated with a bank.
    *
    * @return
    * The summary string.
    */
  def customerSummary: String = {

    customers.foldLeft(SUMMARY_TITLE)(_ + _.summarize)
  }

  override def summarize: String = customerSummary

  /**
    * The total interest paid by this account.
    *
    * @return
    * The total interest.
    */
  def totalInterestPaid: Double = {
    customers.foldLeft(0d)(_ + _.totalInterestEarned)
  }

}


