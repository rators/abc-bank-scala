package com.abc

import com.abc.customer._
import com.abc.transaction.Deposit
import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val account = CheckingAccount("John's Account", List.empty)
    val john: Customer = Customer("John", List(account))
    val bank = Bank(List(john))
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val transactions = List(Deposit(100.0d))
    val checkingAccount = CheckingAccount("Bills's Account", transactions)
    val bill: Customer = Customer("Bill", List(checkingAccount))
    val bank = Bank(List(bill))
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val transactions = List(Deposit(1500.0))
    val checkingAccount: Account = SavingsAccount("Bills's Saving's Account", transactions)
    val bill = Customer("Bill", List(checkingAccount))
    val bank = Bank(List(bill))
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val transactions = List(Deposit(3000.0))
    val checkingAccount: Account = MaxiSavingsAccount("Bills's Maxi-Saving's Account", transactions)
    val bill = Customer("Bill", List(checkingAccount))
    val bank = Bank(List(bill))
    bank.totalInterestPaid should be(170.0)
  }

}
