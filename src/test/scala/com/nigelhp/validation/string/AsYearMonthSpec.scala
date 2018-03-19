package com.nigelhp.validation.string

import java.time.Month.MARCH
import java.time.YearMonth

import com.nigelhp.validation.{Failure, Success}
import org.scalatest.{FreeSpec, Matchers}

class AsYearMonthSpec extends FreeSpec with Matchers {
  "A string representation of a year-month" - {
    "is invalid" - {
      "when empty" in {
        AsYearMonth("") shouldBe Failure("Value [] is not a valid YearMonth")
      }

      "when non-numeric" in {
        AsYearMonth("something-non-numeric") shouldBe Failure("Value [something-non-numeric] is not a valid YearMonth")
      }

      "when numeric but has fewer than six digits" in {
        AsYearMonth("2018") shouldBe Failure("Value [2018] is not a valid YearMonth")
      }

      "when a valid year followed by an invalid month" in {
        AsYearMonth("201813") shouldBe Failure("Value [201813] is not a valid YearMonth")
      }

      "when it complies with the yyyyMMdd format" in {
        AsYearMonth("20180302") shouldBe Failure("Value [20180302] is not a valid YearMonth")
      }
    }

    "is valid" - {
      "when it complies with the yyyyMM format" in {
        AsYearMonth("201803") shouldBe Success(YearMonth.of(2018, MARCH))
      }
    }
  }
}
