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
case class Open(date: LocalDate, memo: String) extends Tx {
    override def amount: Double = 0d
    override def adjust(bal: Balance): Balance = Balance(date, bal.amount)
}
object Tx {
    def apply(d: LocalDate, a: Double, m: String, typeStr: String): Tx = {
        typeStr match {
            case "debit" => Debit(d, a, m)
            case "credit" => Credit(d, a, m)
            case "open" => Open(d, m)
        }
    }
}

case class Account(id: String, interestRatePercent: Double, txs: Seq[Tx], txBalances: Seq[TxBalance], balanceFile: File) {
    def balances: Seq[Balance] = txBalances.map(_.balance)
}

case class Balance(date: LocalDate, amount: Double)

case class TxBalance(tx: Tx, balance: Balance) {
    def date: LocalDate = balance.date
}

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
        val lastTxBalance = acct.txBalances.reverse.find(b => b.date.equals(Utils.mostRecentMonthEnd(b.date))).
            getOrElse(throw new IllegalStateException(s"Expected at least one month-end balance in ${acct.balanceFile}"))
        println(s"Latest month-end balance: $lastTxBalance")
        // compute month (start, end) tuples
        val start = lastTxBalance.date.plusDays(1)
        val end = Utils.mostRecentMonthEnd(through)
        Utils.validateRange(start, end)
        val months = Stream.from(0).map(idx => (start.plusMonths(idx), Utils.monthEnd(start.plusMonths(idx)))).
            takeWhile{case(_, e) => Utils.lessEqual(e, through)}
        // build full list of balances
        // drop head of update, it is dup of last element of bal
        val newBalances = months.scanLeft(Seq(lastTxBalance)){ case(txBals, mTup) =>
            computeInterest(mTup._1, mTup._2, txBals.last, acct)
        }.flatten.tail

        // get immutable history through most recent month-end
        val balanceHistory = acct.txBalances.takeWhile(b => Utils.lessEqual(b.date, lastTxBalance.date))
        // compute current partial period txbal
        val currentBalances = currentTxs(acct, newBalances.last)

        val allBalances = balanceHistory ++ newBalances ++ currentBalances

        //allBalances.foreach(println)

        acct.copy(txBalances = allBalances)
    }

    override def statement(start: LocalDate, end: LocalDate, acct: Account): Statement = {
        Statement(start, end, Seq.empty)
    }

    private def computeInterest(start: LocalDate,
                                end: LocalDate,
                                latestBalance: TxBalance,
                                acct: Account): Seq[TxBalance] = {
        Utils.validateRange(start, end)
        // tx for month
        val txs = acct.txs.filter(tx => between(tx.date, start, end)).
                sortWith{case(tx1, tx2) => tx1.date.compareTo(tx2.date) < 0}
        val txBalances = txs.scanLeft(latestBalance){ case(currTxBal, curTx) =>
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
        txBalances.tail :+ TxBalance(intTx, intTx.adjust(lastBal))
    }

    private def currentTxs(acct: Account, lastBal: TxBalance): Seq[TxBalance] = {
        val txs = acct.txs.filter(tx => tx.date.isAfter(lastBal.date)).
                sortWith{case(tx1, tx2) => tx1.date.compareTo(tx2.date) < 0}
        txs.scanLeft(lastBal){ case(currTxBal, curTx) =>
            TxBalance(curTx, curTx.adjust(currTxBal.balance))
        }.tail
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
            Tx(d, a, memo, tokens(1))
        }).toSeq
    }

    private def loadBalances(f: File): Seq[TxBalance] = {
        require(f.exists())
        (for (ln <- Source.fromFile(f).getLines().drop(1)) yield {
            val tokens = ln.split(",")
            // [0] date
            // [1] txtype
            // [2] txamount
            // [3] txmemo
            // [4] balance
            val d = LocalDate.parse(tokens(0))
            val balAmt = tokens(4).toDouble
            val txAmt = tokens(2).toDouble
            val tx = Tx(d, txAmt, tokens(3), tokens(1))
            val b = Balance(d, balAmt)
            TxBalance(tx, b)
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
