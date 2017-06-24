package com.tstuttle

import java.io.File
import java.time.LocalDate

import scala.io.Source
import scala.util.{Success, Try}
import Utils.{between, lessEqual}

import scala.collection.SortedMap

sealed trait Tx {
    def date: LocalDate
    def amount: Double
    def memo: String
    def adjust(bal: Balance): Balance
}
case class Credit(date: LocalDate, amount: Double, memo: String) extends Tx {
    override def adjust(bal: Balance): Balance = Balance(date, amount + bal.amount)
}
case class Debit(date: LocalDate, amount: Double, memo: String) extends Tx {
    override def adjust(bal: Balance): Balance = Balance(date, bal.amount - amount)
}
case class Interest(date: LocalDate, amount: Double, memo: String) extends Tx {
    override def adjust(bal: Balance): Balance = Balance(date, amount + bal.amount)
}

case class Account(id: String, interestRatePercent: Double, txs: Seq[Tx], balances: Seq[Balance])

case class Balance(date: LocalDate, amount: Double)

case class TxBalance(tx: Tx, balance: Balance)

trait AccountService {
    def computeInterest(start: LocalDate, end: LocalDate, acct: Account): Try[Account]
    def statement(account: Account) : Seq[TxBalance]
}

trait AccountRepository {
    def query(id: String): Option[Account]
    def store(a: Account): Try[Account]
}

object AccountServiceImpl extends AccountService {

    override def computeInterest(start: LocalDate, end: LocalDate, acct: Account): Try[Account] = {
        Try{
            Utils.validate(start, end)
            doComputeInterest(start, end, acct)
        }
    }

    private def doComputeInterest(start: LocalDate, end: LocalDate, acct: Account): Account = {
        val latestBalance = acct.balances.find(b => b.date == start).getOrElse(Balance(start, 0d))
        val txs = acct.txs.filter(tx => between(tx.date, start, end)).
                sortWith{case(tx1, tx2) => tx1.date.compareTo(tx2.date) < 0}
        val balances = txs.scanLeft(latestBalance){ case(currBal, curTx) =>
            curTx.adjust(currBal)
        }
        val days = Stream.from(0).map(idx => start.plusDays(idx)).
                takeWhile(d => lessEqual(d, end))
        val balDesc = balances.reverse
        val byDay: Seq[(LocalDate, Balance)] = days.flatMap{ d =>
            val balOpt = balDesc.find(b => lessEqual(b.date, d));
            balOpt.map(b => (d, b))
        }
        val balAmounts = byDay.map{case(_, b) => b.amount}
        val numDays = balAmounts.size
        val intTx: Option[Tx] = for {
            avgDailyBal <- balAmounts.reduceOption(_+_).map(_/numDays)
        } yield Interest(end,
            avgDailyBal*acct.interestRatePercent/100d*(numDays/365d),
            s"rate=${acct.interestRatePercent}% avgDailyBal=$avgDailyBal days=$numDays")
        val lastBal = byDay.last._2
        val balWithInt = intTx.map(intTx => balances :+ intTx.adjust(lastBal)).getOrElse(balances)
        val txsWitInt = intTx.map(intTx => acct.txs :+ intTx).getOrElse(acct.txs)
        acct.copy(txs = txsWitInt, balances = balWithInt)
    }

    override def statement(account: Account): Seq[TxBalance] = {
        Seq()
    }
}

class FileAccountRepository(file: File) extends AccountRepository {

    lazy val accts: Seq[Account] = loadAccounts(file)

    override def query(id: String): Option[Account] = {
        accts.find(_.id == id)
    }

    private def loadTxs(f: File): Seq[Tx] = {
        require(f.exists())
        (for (ln <- Source.fromFile(f).getLines().drop(1)) yield {
            val tokens = ln.split(",")
            val d = LocalDate.parse(tokens(0))
            val a = tokens(2).toDouble
            val memo = tokens.lift(3).getOrElse("")
            tokens(1) match {
                case "debit" => Debit(d, a, memo)
                case "credit" => Credit(d, a, memo)
            }
        }).toSeq
    }

    private def loadBalances(f: File): Seq[Balance] = {
        require(f.exists())
        (for (ln <- Source.fromFile(f).getLines().drop(1)) yield {
            val tokens = ln.split(",")
            val d = LocalDate.parse(tokens(0))
            val a = tokens(1).toDouble
            Balance(d, a)
        }).toSeq
    }

    private def loadAccounts(f: File): Seq[Account] = {
        require(f.exists())
        (for (ln <- Source.fromFile(f).getLines().drop(1)) yield {
            val tokens = ln.split(",")
            val id = tokens(0)
            val rate = tokens(1).toDouble
            val txFile = new File(f.getParentFile, tokens(2))
            val txs = loadTxs(txFile)
            val balFile = new File(f.getParentFile, tokens(3))
            val bals = loadBalances(balFile)
            Account(id, rate, txs, bals)
        }).toSeq
    }

    override def store(a: Account): Try[Account] = {
        Success(null)
    }
}

object App {
    def main(args: Array[String]): Unit = {

    }
}
