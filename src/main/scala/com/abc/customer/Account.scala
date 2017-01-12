package com.abc.customer


import java.time.format.DateTimeFormatter

import com.abc.customer.Account.TransactionLog
import com.abc.transaction._
import com.abc.utils.DateProvider
import com.abc.utils.Formatting._
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, Days}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.Failure

object Account {
  /**
    * The transaction log type alias.
    */
  type TransactionLog = List[Transaction]

  final val FAILURE_ERROR = Failure(new IllegalArgumentException("amount must be greater than zero"))

  def interestEarned(annualRate: Float, currBalance: Double): Double = {
    val dailyRate = annualRate / 365d
    println(s"the balance[$currBalance] * dailyRate[$dailyRate] = ${currBalance * dailyRate}")
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
  final def sumTransactions(transaction: Iterable[Transaction]): Double = transaction.foldLeft(0d)(_ + _.amount)

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
  def withdrawalOrDepositText(t: Transaction): String = {
    t match {
      case _: Withdrawal => "withdrawal"
      case _: Deposit => "deposit"
      case _: Transfer[_] => "transfer"
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

  def groupLogByDay: Map[DateTime, List[Transaction]] = transactions.groupBy(_.transactionDate)

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

  override def transactions: List[Transaction] = _transactions toList

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

  def dateMap(dateList: List[DateTime]): Map[DateTime, () => mutable.Buffer[Transaction]] = {
    val dateMap: Map[DateTime, () => mutable.Buffer[Transaction]] = dateList.map((date) => (date, () => _transactions.filter((t) => AccountsTest.isSameDay(t.transactionDate, date)))).toMap

    //date range contains all the dates by day that have passed from the first transaction up until now
    //date map has the past days as keys associated with the list value, the list contains all transactions that
    //occurred before that date
    dateMap
  }

  def dateTimesList: List[DateTime] = {
    val days: Days = DateProvider.dayRange(AccountsTest.dt, DateProvider.now)
    val daysRange = days.getDays until 0 by -1
    val now = DateProvider.now
    val dtl = daysRange.map((count) => now.minusDays(count)).toList
    dtl
  }

  def DCIRec(dayCount: Int, balance: Double): Double = {
    dayCount match {
      case 0 => balance
      case _ =>
        val intEarned = balance * (1d +.001d / 365d)
        DCIRec(dayCount - 1, intEarned)
    }
  }

  @tailrec
  final def DCIRec(dayCount: List[DateTime], dayMap: Map[DateTime, () => Iterable[Transaction]], prevBalance: Double): Double = {
    dayCount match {
      case Nil => prevBalance
      case head :: tail =>
        val newBalance: Double = Account.sumTransactions(dayMap(head)()) + prevBalance
        val intEarned = newBalance * (1d +.001d / dayMap.size)
        DCIRec(tail, dayMap, intEarned)
    }
  }

  def dailyCompoundInterest: Double = {
    val dateList = dateTimesList
    DCIRec(dateList, dateMap(dateList), 0)
  }
}

/**
  * Companion object for checking account.
  */
object CheckingAccount {
  def apply(name: String, __transactions: TransactionLog): CheckingAccount = new CheckingAccount(name, __transactions)

  def unapply(arg: CheckingAccount): Option[(String, TransactionLog)] = Some {
    (arg.name, arg.transactions)
  }
}

/**
  * The savings account case class representation
  *
  * @param name
  * The name for this account.
  * @param __transactions
  * The transaction log associated with this account.
  */
class SavingsAccount(override val name: String, __transactions: TransactionLog) extends Account {
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
  * Companion object for savings account.
  */
object SavingsAccount {
  def apply(name: String, __transactions: TransactionLog): CheckingAccount = new CheckingAccount(name, __transactions)

  def unapply(arg: SavingsAccount): Option[(String, TransactionLog)] = Some {
    (arg.name, arg.transactions)
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
class MaxiSavingsAccount(override val name: String, __transactions: TransactionLog) extends Account {
  private val _transactions: scala.collection.mutable.Buffer[Transaction] = __transactions.toBuffer

  override def transactions: List[Transaction] = _transactions.toList

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
    retLog.toList
  }

  def ++=(transactionsIn: TransactionLog): TransactionLog = {
    val retLog = _transactions ++= transactionsIn
    retLog.toList
  }
}

/**
  * Companion object for savings account.
  */
object MaxiSavingsAccount {
  def apply(name: String, __transactions: TransactionLog): CheckingAccount = new CheckingAccount(name, __transactions)

  def unapply(arg: SavingsAccount): Option[(String, TransactionLog)] = Some {
    (arg.name, arg.transactions)
  }
}

object AccountsTest extends App {
  lazy val dt = DateProvider.now.minusYears(1)
  lazy val fmt = DateTimeFormat.forPattern("yyyyMMdd")

  def isSameDay(dateA: DateTime, dateB: DateTime): Boolean = {
    fmt.print(dateA) equals fmt.print(dateB)
  }

  val accountsTest = CheckingAccount("Test", List.empty)
  accountsTest.addTransaction(TestDeposit(2000, dt))
  val regTotal = Account.sumTransactions(accountsTest.transactions)
  val dateList = accountsTest.dateTimesList
  val intEarnedRecurse = accountsTest.dailyCompoundInterest

  println(f"Interest earned: ${intEarnedRecurse - regTotal}%.2f")

}