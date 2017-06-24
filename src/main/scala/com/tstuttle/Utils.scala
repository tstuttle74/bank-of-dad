package com.tstuttle

import java.time.LocalDate

/**
  * Created by ted on 4/16/2017.
  */
object Utils {

    def validate(s: LocalDate, e: LocalDate): Unit = {
        require(s.compareTo(e) <= 0, s"start $s is after end $e")
    }

    def between (d: LocalDate, s: LocalDate, e: LocalDate): Boolean = {
        (d.isAfter(s) || d.equals(s)) && (d.equals(e) || d.isBefore(e))
    }

    def lessEqual(d: LocalDate, e: LocalDate): Boolean = d.equals(e) || d.isBefore(e)

}
