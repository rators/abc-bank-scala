package com.abc.utils

import com.abc.customer.{Account, Customer}
import com.abc.customer.Account.sumTransactions
import com.abc.utils.Formatting.toDollars

/**
  * Makes statements.
  */
object StatementMaker {

  /**
    * Retrieves the sum of the transactions associated with an account.
    *
    * @param a
    * The account whose transactions are being summed.
    * @return
    * The sum of the transactions associated with an account.
    */
  private def sumAccount(a: Account) = sumTransactions(a.transactions)

  /**
    * Retrieves the statement associated with a customer.
    *
    * @param c
    * The customer whose statement is being retrieved.
    * @return
    * The statement string associated with a customer.
    */
  def customerToStatement(c: Customer) = {
    val totalAcrossAllAccounts = c.accounts.foldLeft(0d)(_ + sumAccount(_))

    f"Statement for ${c.name}\n" +
      c.accounts.map(_.statement).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }
}
