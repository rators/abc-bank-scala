package com.abc

import com.abc.customer.{Account, Customer}
import com.abc.transaction.{Deposit, Transaction, Transfer, Withdrawal}

import scala.util.{Failure, Success, Try}

/**
  * The transfer slip associated with a transfer transaction.
  *
  * @param fromLog
  * The log for the 'from' account resulting form the transaction.
  * @param toLog
  * The log for the 'to' account resulting form the transaction.
  */
case class TransferSlip(fromLog: List[Transaction], toLog: List[Transaction])

/**
  * The receipt for a customer's transaction.
  *
  * @param customer
  * The customer associated with this transfer.
  * @param transferSlip
  * The slip containing the log information for this transfer.
  */
case class TransferReceipt(customer: Customer, transferSlip: TransferSlip)

/**
  * Teller object that handles transactions between customers within the bank.
  */
object Teller {

  /**
    * Transfers funds from one account to another.
    *
    * @param c
    * The customer associated with the accounts involved in the transfer
    * @param from
    * The account funds are being WITHDRAWN from.
    * @param to
    * The account funds are being DEPOSITED into.
    * @param amount
    * The amount being transfer from the 'from' account to the 'to' account parameters.
    * @return
    * The transfer receipts.
    */
  def transferFunds(c: Customer)(from: Account, to: Account, amount: Double): TransferReceipt = {
    val fromTransfer = Transfer(Withdrawal(amount), to)
    val toTransfer = Transfer(Deposit(amount), from)

    val fromLog = from.transactions :+ fromTransfer
    val toLog = to.transactions :+ toTransfer

    val keepAccounts: List[Account] = c.accounts.filter((a) => a == from || a == to)

    val accountsList: List[Account] = keepAccounts :+ from.copy(fromLog) :+ to.copy(toLog)

    val slip = TransferSlip(fromLog, toLog)

    TransferReceipt(Customer(c.name, accountsList), slip)
  }

}
