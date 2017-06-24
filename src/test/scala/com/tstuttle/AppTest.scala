package com.tstuttle

import java.io.File
import java.time.LocalDate

import org.junit.Test
import org.junit.Assert._

import scala.util.Try

/**
  * Created by ted on 4/14/2017.
  */
class AppTest {

    @Test
    def testDateUtil(): Unit = {
        val s = LocalDate.parse("2016-12-31")
        val e = LocalDate.parse("2017-01-01")
        Utils.validate(s, e)
        val result = Try (Utils.validate(e, s))
        assert(result.isFailure)
    }

    @Test
    def testTx(): Unit = {
        val d = LocalDate.parse("2016-12-31")
        val c = Credit(d, 100d, "m1")
        val de = Debit(d, 50d, "m1")
        val b = Balance(d, 100d)
        assert(200d == c.adjust(b).amount)
        assert(50d == de.adjust(b).amount)
    }

    @Test
    def testBasic(): Unit = {
        val file = new File(new File("C:/Users/ted/Google\ Drive/teds-stuff/bank-of-dad"), "content/accounts.csv")
        val repo = new FileAccountRepository(file)
        val acct = repo.query("henry")
        assertNotNull(acct)
        assertTrue(acct.isDefined)
        val updated = AccountServiceImpl.computeInterest(
            LocalDate.parse("2016-07-31"), LocalDate.parse("2017-05-07"), acct.get)
        for { a <- updated
        } {
            a.txs.foreach(println)
            a.balances.foreach(println)
        }
    }
}
