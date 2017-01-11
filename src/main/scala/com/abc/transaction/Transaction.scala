package com.abc.transaction

import java.sql.Date

import com.abc.customer.Account
import com.abc.utils.DateProvider
import org.joda.time.DateTime

import scala.util.{Failure, Success, Try}

/**
  * The transaction trait category.
  */
sealed trait Transaction {

  /**
    * The amount of money associated with this transaction.
    */
  val amount: Double

  /**
    * The date this transaction took place.
    */
  val transactionDate = DateProvider.now
}

/**
  * A withdrawal transaction case class representation.
  *
  * @param amount
  * The amount of money withdrawn during this transaction.
  */
case class TestDeposit(override val amount: Double, override val transactionDate: DateTime) extends StandardTransaction {
  Transaction.checkAmount(amount)

  override def toString = s"TestTransaction(amount=$amount)"
}



/**
  * The transaction companion object. Contains convenience methods.
  */
object Transaction {

  /**
    * Convenience method for verifying that some amount is a positive value.
    *
    * @param amount
    * Some transaction amount.
    * @return
    * A success if and only if the amount is a positive value.
    */
  def checkAmount(amount: Double): Try[Double] = {
    amount match {
      case it if amount <= 0 => Failure(throw new IllegalArgumentException("Negative transaction values not allowed."))
      case _ => Success(amount)
    }
  }

}

/**
  * The trait category for a standard transaction.
  */
trait StandardTransaction extends Transaction

/**
  * A withdrawal transaction case class representation.
  *
  * @param amount
  * The amount of money withdrawn during this transaction.
  */
class Withdrawal(override val amount: Double) extends StandardTransaction {
  Transaction.checkAmount(amount)

  override def toString = s"Withdrawal(amount=$amount)"
}

object Withdrawal {
  def apply(amount: Double) = {
    Transaction.checkAmount(amount)
    new Withdrawal(- amount)
  }

  def unapply(arg: Withdrawal): Option[Double] = Some(arg.amount)
}

/**
  * A deposit transaction case class representation.
  *
  * @param amount
  * The amount of money deposited during this transaction.
  */
case class Deposit(override val amount: Double) extends StandardTransaction {
  Transaction.checkAmount(amount)

  override def toString = s"Deposit(amount=$amount)"
}

/**
  * A transfer transaction case class representation.
  *
  * @param transaction
  * The action taken during the transfer relevant to this account.
  * ex: If funds [x] were withdrawn from this account [a] and deposited into another account [n]
  * then transactions for a will contain will be Transfer(Withdrawal(x), n)
  * @param account
  * The account funds were transferred from, or transferred into.
  * @tparam T
  * The type of standard transaction for this transfer associated with this account.
  */
class Transfer[T <: StandardTransaction](val transaction: T, val account: Account) extends Transaction {
  val amount = transaction.amount
}

object Transfer {
  def apply[T <: StandardTransaction](transaction: T, account: Account) = {
    new Transfer(transaction, account)
  }

  def unapply[T <: StandardTransaction](arg: Transfer[T]): Option[(Double, T, Account)] = {
    Some((arg.amount, arg.transaction, arg.account))
  }
}
