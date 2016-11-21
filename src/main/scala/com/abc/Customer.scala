package com.abc

import com.abc.account.{Account, CheckingAccount, MaxiSavingsAccount, SavingsAccount}
import com.abc.account.Account.sumTransactions

import scala.collection.mutable.ListBuffer

class Customer(val name: String, private val _accounts: ListBuffer[Account] = ListBuffer()) {
  def accounts = _accounts.toList

  def openAccount(account: Account): Customer = {
    _accounts += account
    this
  }

  def numberOfAccounts: Int = _accounts.size

  def totalInterestEarned: Double = _accounts.reduce(_.interestEarned + _.interestEarned)

  private def getSum(a: Account) = sumTransactions(a.transactions)

  /**
    * This method gets a statement
    */
  def getStatement: String = {
    val totalAcrossAllAccounts = _accounts.reduce(getSum(_) + getSum(_))

    f"Statement for $name\n" +
      _accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a match {
      case c: CheckingAccount => "Checking Account\n"
      case s: SavingsAccount => "Savings Account\n"
      case m: MaxiSavingsAccount => "Maxi Savings Account\n"
    }

    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")

    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"

    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

