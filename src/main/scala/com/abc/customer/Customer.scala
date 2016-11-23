package com.abc.customer

import com.abc.Summarizable
import com.abc.utils.Formatting._
import com.abc.utils.StatementMaker.customerToStatement

/**
  * Case class representation for a customer.
  *
  * @param name
  * The name for this customer.
  * @param accounts
  * The accounts associated with this customer.
  */
case class Customer(name: String, accounts: List[Account]) extends Summarizable {

  /**
    * The number of accounts associated with this customer.
    *
    * @return
    * The total number of accounts.
    */
  def numberOfAccounts: Int = accounts.size

  /**
    * Total interest earned by this customer through all of their accounts.
    *
    * @return
    * The total interest earned.
    */
  def totalInterestEarned: Double = accounts.foldLeft(0d)(_ + _.interestEarned)

  /**
    * Gets the statement string associated with this account.
    *
    * @return
    * A statement string.
    */
  def statement: String = customerToStatement(this)

  def summarize: String = {
    "\n - " + name + " (" + format(numberOfAccounts, "account") + ")"
  }
}

