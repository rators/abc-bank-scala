package com.abc

import com.abc.customer._
import com.abc.transaction.{Deposit, Withdrawal}
import org.scalatest.{FlatSpec, Matchers}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingTransactions = List(Deposit(100.0))
    val savingsTransactions = List(Deposit(4000.0), Withdrawal(200.0))

    val checkingAccount: Account = CheckingAccount("Henry's Checking Account", checkingTransactions)
    val savingsAccount: Account = SavingsAccount("Henry's Savings Account", savingsTransactions)
    val oscarsAccounts = List(checkingAccount, savingsAccount)
    val henry: Customer = Customer("Henry", oscarsAccounts)

    henry.statement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscarsAccount = List(SavingsAccount("Oscar's Savings Account", List.empty))
    val oscar: Customer = Customer("Oscar", oscarsAccount)
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscarsAccounts = List(
      SavingsAccount("Oscar's Savings Account", List.empty),
      CheckingAccount("Oscar's Checking Account", List.empty)
    )

    val oscar: Customer = Customer("Oscar", oscarsAccounts)
    oscar.numberOfAccounts should be(2)
  }


    ignore should "testThreeAcounts" in {
      val oscarsAccounts: List[Account] = List(
        SavingsAccount("Oscar's Savings Account", List.empty),
        CheckingAccount("Oscar's Checking Account", List.empty),
        MaxiSavingsAccount("Oscar's Max Savings Account", List.empty)
      )

      val oscar: Customer = Customer("Oscar", oscarsAccounts)
      oscar.numberOfAccounts should be(3)
    }
}
