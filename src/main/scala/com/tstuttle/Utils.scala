package com.tstuttle

import java.time.LocalDate

/**
  * Created by ted on 4/16/2017.
  */
object Utils {

    def validateRange(s: LocalDate, e: LocalDate): Unit = {
        require(s.compareTo(e) <= 0, s"start $s is after end $e")
    }

    def between (d: LocalDate, s: LocalDate, e: LocalDate): Boolean = {
        (d.isAfter(s) || d.equals(s)) && (d.equals(e) || d.isBefore(e))
    }

    /**
      *  True if date is equal to or before end
      */
    def lessEqual(date: LocalDate, end: LocalDate): Boolean = date.equals(end) || date.isBefore(end)

    /**
      * Returns end of month for this date
      */
    def monthEnd(date: LocalDate): LocalDate = date.withDayOfMonth(date.lengthOfMonth())

    /**
      * Returns most recent month end #lessEqual to date
      */
    def mostRecentMonthEnd(date: LocalDate): LocalDate = {
        if (date.equals(monthEnd(date))) date
        else monthEnd(date.minusMonths(1))
    }
}
