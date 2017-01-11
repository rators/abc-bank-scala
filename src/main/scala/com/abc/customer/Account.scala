package com.abc.customer

import java.util.Collections

import com.abc.customer.Account.TransactionLog
import com.abc.transaction._
import com.abc.utils.DateProvider
import com.abc.utils.Formatting._
import org.joda.time.{DateTime, Days}

import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.collection.mutable

object Account {
  /**
    * The transaction log type alias.
    */
  type TransactionLog = List[Transaction]

  final val FAILURE_ERROR = Failure(new IllegalArgumentException("amount must be greater than zero"))

  def interestEarned(annualRate: Float, currBalance: Double): Double = {
    val dailyRate = annualRate / 365d
    currBalance * dailyRate
  }

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
  def transactions: TransactionLog

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

  def addTransaction(transaction: Transaction): TransactionLog

  def ++=(transactions: TransactionLog): TransactionLog
}

/**
  * The checking account case class representation
  *
  * @param name
  * The name for this account.
  * @param __transactions
  * The transaction log associated with this account.
  */
class CheckingAccount(override val name: String, __transactions: TransactionLog) extends Account {
  private val _transactions: scala.collection.mutable.Buffer[Transaction] = __transactions.toBuffer

  override def transactions = _transactions toList

  override val title = "Checking Account"

  override def interestEarned: Double = balance * 0.001

  override def copy(log: TransactionLog) = new CheckingAccount(name, log)

  def addTransaction(transactionIn: Transaction): TransactionLog = {
    val retLog = _transactions += transactionIn
    retLog toList
  }

  def ++=(transactionsIn: TransactionLog): TransactionLog = {
    val retLog = _transactions ++= transactionsIn
    retLog toList
  }

  def dateMap(dateList: List[DateTime]): Map[DateTime, mutable.Buffer[Transaction]] = {
    val dateMap: Map[DateTime, mutable.Buffer[Transaction]] = dateList.map((date) => (date, _transactions.filter(_.transactionDate.isBefore(date)))).toMap

    //date range contains all the dates by day that have passed from the first transaction up until now
    //date map has the past days as keys associated with the list value, the list contains all transactions that
    //occurred before that date
    dateMap
  }

  def dateTimesList: List[DateTime] = {
    val days: Days = DateProvider.dayRange(_transactions.head.transactionDate, DateProvider.now)
    val daysRange = days.getDays until 0 by -1
    val now = DateProvider.now
    val dtl = daysRange.map((count) => now.minusDays(count)).toList
    dtl
  }

  def _dailyInterestBalance(dateMap: Map[DateTime, mutable.Buffer[Transaction]])(interestTotal: Double, dateTimes: List[DateTime]): Double = {
    dateTimes match {
      case Nil => interestTotal
      case head :: tail =>
        tail match {
          case Nil =>
            println(s"Interest total: $interestTotal")
            val transactions: mutable.Buffer[Transaction] = dateMap(head)
            val currentSum = Account.sumTransactions(transactions.toList :+ Deposit(10 + interestTotal))
            println(currentSum)
            val nextInterestTotal = Account.interestEarned(0.001f, currentSum)
            println(s"Next interest total: $nextInterestTotal")
            _dailyInterestBalance(dateMap)(nextInterestTotal, tail)
          case notNil :: tailsTail =>
            println(s"Interest total: $interestTotal")
            val transactions: mutable.Buffer[Transaction] = dateMap(head)
            val currentSum = Account.sumTransactions(transactions.toList :+ Deposit(10 + interestTotal))
            println(currentSum)
            val nextInterestTotal = Account.interestEarned(0.001f, currentSum)
            println(s"Next interest total: $nextInterestTotal")
            _dailyInterestBalance(dateMap)(nextInterestTotal, tail)
        }
    }
  }

  def dailyInterestBalance = {
    val dateList = dateTimesList
    _dailyInterestBalance(dateMap(dateList))(0, dateList)
  }
}

object CheckingAccount {
  def apply(name: String, __transactions: TransactionLog): CheckingAccount = new CheckingAccount(name, __transactions)
}

/**
  * The savings account case class representation
  *
  * @param name
  * The name for this account.
  * @param __transactions
  * The transaction log associated with this account.
  */
case class SavingsAccount(override val name: String, __transactions: TransactionLog) extends Account {
  private val _transactions: scala.collection.mutable.Buffer[Transaction] = __transactions.toBuffer

  override def transactions = _transactions.toList

  override val title = "Savings Account"

  override def interestEarned: Double = {
    val total = balance

    total match {
      case it if it <= 1000 => it * 0.001
      case _ => 1 + (total - 1000) * 0.002
    }
  }

  override def copy(log: TransactionLog) = SavingsAccount(name, log)

  def addTransaction(transactionIn: Transaction): TransactionLog = {
    val retLog = _transactions += transactionIn
    retLog toList
  }

  def ++=(transactionsIn: TransactionLog): TransactionLog = {
    val retLog = _transactions ++= transactionsIn
    retLog toList
  }
}

/**
  * The maxi savings account case class representation
  *
  * @param name
  * The name for this account.
  * @param __transactions
  * The transaction log associated with this account.
  */
case class MaxiSavingsAccount(override val name: String, __transactions: TransactionLog) extends Account {
  private val _transactions: scala.collection.mutable.Buffer[Transaction] = __transactions.toBuffer

  override def transactions = _transactions.toList

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

  def addTransaction(transactionIn: Transaction): TransactionLog = {
    val retLog = _transactions += transactionIn
    retLog toList
  }

  def ++=(transactionsIn: TransactionLog): TransactionLog = {
    val retLog = _transactions ++= transactionsIn
    retLog toList
  }
}

object AccountsTest extends App {
  val accountsTest = CheckingAccount("Test", List.empty)

  val dt = DateProvider.now.minusYears(1)
  accountsTest.addTransaction(TestDeposit(10d, dt))
  println(accountsTest.dailyInterestBalance)
}