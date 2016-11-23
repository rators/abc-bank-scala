package com.abc.customer

import com.abc.customer.Account.TransactionLog
import com.abc.transaction.{Deposit, Transaction, Transfer, Withdrawal}

import scala.util.Failure
import com.abc.utils.Formatting._

object Account {
  /**
    * The transaction log type alias.
    */
  type TransactionLog = List[Transaction]

  final val FAILURE_ERROR = Failure(new IllegalArgumentException("amount must be greater than zero"))

  /**
    * Sums a list of transactions.
    *
    * @param transaction
    * The transaction associated with some account.
    * @return
    * The total balance.
    */
  final def sumTransactions(transaction: List[Transaction]): Double = transaction.foldLeft(0d)(_ + _.amount)

  /**
    * Creates a statement for an account.
    *
    * @param a
    * An account.
    * @return
    * The statement string asocciated with an account.
    */
  def statementForAccount(a: Account): String = {
    val accountType = a.title

    val transactionSummary = a.transactions
      .map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("\n  ", "\n  ", "\n")

    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"

    accountType + transactionSummary + totalSummary
  }

  /**
    * Retrieves the lower-case title text for a transaction.
    *
    * @param t
    * A transaction.
    * @return
    * The text associated with a transaction.
    */
  def withdrawalOrDepositText(t: Transaction) = {
    t match {
      case w: Withdrawal => "withdrawal"
      case d: Deposit => "deposit"
      case tf: Transfer[_] => "transfer"
    }
  }
}

/**
  * The account trait. Defines the interface for all classes typed in the account category.
  */
sealed trait Account {

  /**
    * The name for this account.
    */
  val name: String

  /**
    * The tile of this account. ie. 'Maxi Savings Account' for a MaxiSavingsAccount
    */
  val title: String

  /**
    * The transactions associated with this account.
    */
  val transactions: TransactionLog

  /**
    * The total interest earned by this account.
    *
    * @return
    * Total interest.
    */
  def interestEarned: Double

  /**
    * The statement text for this account.
    *
    * @return
    * The statement text.
    */
  def statement: String = Account.statementForAccount(this)

  /**
    * The balance for this account.
    *
    * @return
    * The account balance.
    */
  def balance: Double = Account.sumTransactions(transactions)

  /**
    * Creates a copy of this Account from a transaction log.
    *
    * @param log
    * The log to create a copy from.
    * @return
    * An account copy.
    */
  def copy(log: TransactionLog): Account

  def groupLogByDay = transactions.groupBy(_.transactionDate)
}

/**
  * The checking account case class representation
  *
  * @param name
  * The name for this account.
  * @param transactions
  * The transaction log associated with this account.
  */
case class CheckingAccount(override val name: String, transactions: TransactionLog) extends Account {
  override val title = "Checking Account"

  override def interestEarned: Double = balance * 0.001

  override def copy(log: TransactionLog) = CheckingAccount(name, log)
}

/**
  * The savings account case class representation
  *
  * @param name
  * The name for this account.
  * @param transactions
  * The transaction log associated with this account.
  */
case class SavingsAccount(override val name: String, transactions: TransactionLog) extends Account {
  override val title = "Savings Account"

  override def interestEarned: Double = {
    val total = balance

    total match {
      case it if it <= 1000 => it * 0.001
      case _ => 1 + (total - 1000) * 0.002
    }
  }

  override def copy(log: TransactionLog) = SavingsAccount(name, log)

}

/**
  * The maxi savings account case class representation
  *
  * @param name
  * The name for this account.
  * @param transactions
  * The transaction log associated with this account.
  */
case class MaxiSavingsAccount(override val name: String, transactions: TransactionLog) extends Account {
  override val title = "Maxi Savings Account"

  override def interestEarned: Double = {
    val amount = balance

    amount match {
      case it if it <= 1000 => amount * 0.02
      case it if it <= 2000 => 20 + (amount - 1000) * 0.05
      case _ => 70 + (amount - 2000) * 0.1
    }
  }

  override def copy(log: TransactionLog) = MaxiSavingsAccount(name, log)
}