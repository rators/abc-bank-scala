package com.abc

import com.abc.Bank.{CustomerMap, contentsToMap}
import com.abc.customer.Customer

trait Institution {
  def customers: Map[String, Customer]
}

/**
  * The case class representation for a bank.
  *
  * @param customersList
  * The customers associated with this bank.
  */
case class Bank(customersList: List[Customer]) extends Summarizable {
  val SUMMARY_TITLE: String = "Customer Summary"

  private val customerMap: CustomerMap = contentsToMap(customersList.map((c) => (c.name, c)))

  /**
    * The summary associated with a bank.
    *
    * @return
    * The summary string.
    */
  def customerSummary: String = {
    customerMap.values.foldLeft(SUMMARY_TITLE)(_ + _.summarize)
  }

  override def summarize: String = customerSummary

  /**
    * The total interest paid by this account.
    *
    * @return
    * The total interest.
    */
  def totalInterestPaid: Double = {
    customerMap.values.foldLeft(0d)(_ + _.totalInterestEarned)
  }

  def addCustomer(customer: Customer): this.type = {
    customerMap += customer.name -> customer
    this
  }

}

object Bank {
  type CustomerMap = scala.collection.mutable.LinkedHashMap[String, Customer]
  def contentsToMap(c: Seq[(String, Customer)]) = new scala.collection.mutable.LinkedHashMap[String, Customer]() ++= c
}


