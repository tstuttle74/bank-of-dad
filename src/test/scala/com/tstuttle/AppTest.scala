package com.tstuttle

import java.io.File
import java.time.LocalDate
import java.time.LocalDate.parse

import org.junit.Test
import org.junit.Assert._

import scala.util.Try

/**
  * Created by ted on 4/14/2017.
  */
class AppTest {

    @Test
    def testDateUtil(): Unit = {
        val s = parse("2016-12-31")
        val e = parse("2017-01-01")
        Utils.validateRange(s, e)
        val result = Try(Utils.validateRange(e, s))
        assert(result.isFailure)
    }

    @Test
    def testMoreDateUtil(): Unit = {
        val s = parse("2016-12-31")
        val e = parse("2017-01-01")
        Utils.validateRange(s, e)
        val result = Try(Utils.validateRange(e, s))
        assert(result.isFailure)
    }

    @Test
    def testTx(): Unit = {
        val d = parse("2016-12-31")
        val c = Credit(d, 100d, "m1")
        val de = Debit(d, 50d, "m1")
        val b = Balance(d, 100d)
        assert(200d == c.adjust(b).amount)
        assert(50d == de.adjust(b).amount)
    }

    @Test
    def testBalance(): Unit = {
        Seq(
            Balance(parse("2016-01-15"), 1d),
            Balance(parse("2016-01-21"), 2d),
            Balance(parse("2016-01-31"), 3d),
            Balance(parse("2016-02-15"), 4d)
        )
    }

    @Test
    def testMonthEnd(): Unit = {
        assert(parse("2016-01-31") == Utils.monthEnd(parse("2016-01-01")))
        assert(parse("2016-01-31") == Utils.monthEnd(parse("2016-01-31")))
        assert(parse("2016-01-31") == Utils.mostRecentMonthEnd(parse("2016-02-15")))
        assert(parse("2016-01-31") == Utils.mostRecentMonthEnd(parse("2016-01-31")))
    }
}