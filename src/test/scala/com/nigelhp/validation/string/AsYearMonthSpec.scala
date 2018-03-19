package com.nigelhp.validation.string

import java.time.Month.MARCH
import java.time.YearMonth

import com.nigelhp.validation.{Failure, Success}
import org.scalatest.{FreeSpec, Matchers}

class AsYearMonthSpec extends FreeSpec with Matchers {
  "A string representation of a year-month" - {
    "is invalid" - {
      "when empty" in {
        AsYearMonth("") shouldBe Failure("Value [] does not comply with yearMonth format [uuuuMM]")
      }

      "when non-numeric" in {
        AsYearMonth("something-non-numeric") shouldBe Failure("Value [something-non-numeric] does not comply with yearMonth format [uuuuMM]")
      }

      "when numeric but having fewer than six digits" in {
        AsYearMonth("2018") shouldBe Failure("Value [2018] does not comply with yearMonth format [uuuuMM]")
      }

      "when a valid year followed by an invalid month" in {
        AsYearMonth("201813") shouldBe Failure("Value [201813] does not comply with yearMonth format [uuuuMM]")
      }

      "when it complies with the yyyyMMdd format" in {
        AsYearMonth("20180302") shouldBe Failure("Value [20180302] does not comply with yearMonth format [uuuuMM]")
      }
    }

    "is valid" - {
      "when it complies with the uuuuMM format" in {
        AsYearMonth("201803") shouldBe Success(YearMonth.of(2018, MARCH))
      }
    }
  }
}
