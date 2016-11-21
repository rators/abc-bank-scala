package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  private val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) = {
    val currCustomerState = customers += customer
    currCustomerState.toList
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.reduce(_.totalInterestEarned + _.totalInterestEarned)
  }

}


