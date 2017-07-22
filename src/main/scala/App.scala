package com.tstuttle

import java.io.{File, StringWriter, Writer}
import java.time.LocalDate

import scala.io.Source
import scala.util.{Success, Try}
import Utils.{between, lessEqual}

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

case class Account(id: String, interestRatePercent: Double, txs: Seq[Tx], balances: Seq[Balance], balanceFile: File)

case class Balance(date: LocalDate, amount: Double)

case class TxBalance(tx: Tx, balance: Balance)

case class Statement(start: LocalDate, end: LocalDate, details: Seq[TxBalance])

trait AccountService {
    def updateBalances(through: LocalDate, acct: Account): Account
    def statement(start: LocalDate, end: LocalDate, acct: Account) : Statement
    def print(acct: Account, stmt: Statement, writer: Writer): Unit
}

trait AccountRepository {
    def query(id: String): Option[Account]
    def store(a: Account): Try[Account]
}

object AccountServiceImpl extends AccountService {

    override def print(acct: Account, stmt: Statement, writer: Writer): Unit = {

    }

    override def updateBalances(through: LocalDate, acct: Account): Account = {
        // find most recent month-end balance
        val lastMonthEndBalance = acct.balances.reverse.find(b => b.date.equals(Utils.mostRecentMonthEnd(b.date))).
            getOrElse(throw new IllegalStateException(s"Expected at least one month-end balance in ${acct.balanceFile}"))
        println(s"Latest month-end balance: $lastMonthEndBalance")
        // get immutable history through most recent month-end
        val balanceHistory = acct.balances.takeWhile(b => Utils.lessEqual(b.date, lastMonthEndBalance.date))
        // compute month (start, end) tuples
        val start = lastMonthEndBalance.date.plusDays(1)
        val end = Utils.mostRecentMonthEnd(through)
        Utils.validateRange(start, end)
        val months = Stream.from(0).map(idx => (start.plusMonths(idx), Utils.monthEnd(start.plusMonths(idx)))).
            takeWhile{case(_, e) => Utils.lessEqual(e, through)}
        // compute month-end balances w/ interest
        var startingBal = lastMonthEndBalance
        val balances = (for ((mStart, mEnd) <- months) yield {
            val monthBalances = computeInterest(mStart, mEnd, startingBal, acct)
            startingBal = monthBalances.last.balance
            monthBalances
        }).flatten

        balances.foreach(println)

//        // assume if we have a balances it includes all tx for that date
//        val txs = acct.txs.filter(tx => tx.date.isAfter(latestBalance.date) && Utils.lessEqual(tx.date, through)).
//            sortWith{case(tx1, tx2) => tx1.date.compareTo(tx2.date) < 0}
//        val balances = txs.scanLeft(latestBalance){ case(currBal, curTx) =>
//            curTx.adjust(currBal)
//        }

        acct
    }

    override def statement(start: LocalDate, end: LocalDate, acct: Account): Statement = {
        Statement(start, end, Seq.empty)
    }

    private def computeInterest(start: LocalDate,
                                end: LocalDate,
                                latestBalance: Balance,
                                acct: Account): Seq[TxBalance] = {
        Utils.validateRange(start, end)
        // tx for month
        val txs = acct.txs.filter(tx => between(tx.date, start, end)).
                sortWith{case(tx1, tx2) => tx1.date.compareTo(tx2.date) < 0}
        val txBalances = txs.scanLeft(TxBalance(null, latestBalance)){ case(currTxBal, curTx) =>
            TxBalance(curTx, curTx.adjust(currTxBal.balance))
        }
        val balances = txBalances.map(_.balance)
        val days = Stream.from(0).map(idx => start.plusDays(idx)).
                takeWhile(d => lessEqual(d, end))
        val balDesc = balances.reverse
        val byDay: Seq[(LocalDate, Balance)] = days.flatMap{ d =>
            val balOpt = balDesc.find(b => lessEqual(b.date, d))
            balOpt.map(b => (d, b))
        }
        val balAmounts = byDay.map{case(_, b) => b.amount}
        val sumBal = balAmounts.sum
        val numDays = balAmounts.size
        val avgDailyBal = sumBal / numDays
        val intTx = Interest(end,
            avgDailyBal*acct.interestRatePercent/100d*(numDays/365d),
            s"rate=${acct.interestRatePercent}% avgDailyBal=$avgDailyBal days=$numDays")
        val lastBal = byDay.last._2
        txBalances :+ TxBalance(intTx, intTx.adjust(lastBal))
    }
}

class FileAccountRepository(file: File) extends AccountRepository {

    require(file.exists())

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
            Account(id, rate, txs, bals, balFile)
        }).toSeq
    }

    override def store(a: Account): Try[Account] = {

        Success(null)
    }
}

object App {
    def main(args: Array[String]): Unit = {

        // arguments
        val file = new File(args(0))
        val user = args(1)
        val start = LocalDate.parse(args(2))
        val end = LocalDate.parse(args(3))

        // validate
        require(file.exists(), s"Missing account repo $file")
        Utils.validateRange(start, end)

        // main program logic
        val repo = new FileAccountRepository(file)
        val acct = repo.query(user).getOrElse(throw new IllegalArgumentException(s"Failed to locate account for $user"))
        val newAcct = AccountServiceImpl.updateBalances(end, acct)
        val stmt = AccountServiceImpl.statement(start,  end, newAcct)

        // side-effects
        repo.store(newAcct)
        val writer = new StringWriter()
        AccountServiceImpl.print(newAcct, stmt, writer)
        println(writer.toString)
    }
}
