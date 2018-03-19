package com.nigelhp.validation.conversion

import java.time.YearMonth
import java.time.format.DateTimeFormatter

import com.nigelhp.validation.{Validation, ValidationWithMessage}

import scala.util.Try

object StringAsYearMonth {
  /*
   * NOTE: use of 'u' (year) rather than 'y' (year-of-era).
   * This prevents 20180302 from being interpreted as the 2nd month of the year 201803.
   */
  private val YearMonthFormat = "uuuuMM"

  def tryParseYearMonth(value: String): Try[YearMonth] =
    Try(YearMonth.parse(value, DateTimeFormatter.ofPattern(YearMonthFormat)))

  def apply(value: String): ValidationWithMessage[YearMonth] =
    Validation.fromTry(_ => s"Value [$value] does not comply with yearMonth format [$YearMonthFormat]") {
      tryParseYearMonth(value)
    }
}
