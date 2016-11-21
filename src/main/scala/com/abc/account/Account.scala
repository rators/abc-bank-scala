package com.abc.account

import com.abc.Transaction

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
  final val FAILURE_MESSAGE = Failure(new IllegalArgumentException("amount must be greater than zero"))

  final def sumTransactions(transaction: Iterable[Transaction]) = transaction.reduce(_.amount + _.amount)
}

sealed trait Account {
  val transactions: ListBuffer[Transaction] = ListBuffer()
  val name: String

  def deposit(amount: Double): Try[List[Transaction]] = {
    if (amount <= 0)
      Failure(new IllegalArgumentException("amount must be greater than zero"))
    else
      transactions += Transaction(amount.abs)
    Success(transactions.toList)
  }

  def withdraw(amount: Double): Try[List[Transaction]] = {
    if (amount <= 0)
      Success(new IllegalArgumentException("amount must be greater than zero"))
    else
      transactions += Transaction(-amount.abs)
    Success(transactions.toList)
  }

  def interestEarned: Double
}

class CheckingAccount(override val name: String, override val transactions: ListBuffer[Transaction]) extends Account {
  override def interestEarned: Double = Account.sumTransactions(transactions) * 0.001
}

class SavingsAccount(override val name: String, override val transactions: ListBuffer[Transaction]) extends Account {
  override def interestEarned: Double = {
    val amount = Account.sumTransactions(transactions)

    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

class MaxiSavingsAccount(override val name: String, override val transactions: ListBuffer[Transaction]) extends Account {
  override def interestEarned: Double = {
    val amount = Account.sumTransactions(transactions)

    if (amount <= 1000) return amount * 0.02
    if (amount <= 2000) return 20 + (amount - 1000) * 0.05
    70 + (amount - 2000) * 0.1
  }
}