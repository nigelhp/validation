package com.nigelhp.validation

import java.time.Month.MARCH
import java.time.YearMonth

import com.nigelhp.validation.constraint.StringSatisfiesMinimumLength
import com.nigelhp.validation.conversion.StringAsYearMonth
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Try

class WorkedExampleSpec extends FreeSpec with Matchers {

  "An ID string" - {
    "can be validated for its length" - {
      "when valid" in {
        StringSatisfiesMinimumLength(4)("abcd  ".trim) shouldBe Success("abcd")
      }

      "when invalid" in {
        StringSatisfiesMinimumLength(4)("ab  ".trim) shouldBe Failure("String [ab] does not satisfy minimum length of [4]")
      }
    }
  }

  /*
   * If we were using the "tiny type" pattern, we might want to convert a valid string into an ID wrapper.
   * When the conversion is "safe" (cannot throw an exception) we can use map.
   */
  case class Id(value: String)

  "An ID tiny type" - {
    "can be created when valid" in {
      Validation.map(Success("abcd"))(Id) shouldBe Success(Id("abcd"))
    }

    "cannot be created when invalid" in {
      val failure = Failure("String [ab] does not satisfy minimum length of [4]")

      Validation.map(failure)(Id) shouldBe failure
    }
  }

  /*
   * However, we cannot use this approach when a conversion is not "safe" (may throw an exception).
   *
   * Assume that IDs, in addition to having a minimum length, must be numeric.
   */
  "An ID satisfying the minimum length requirements" - {
    "cannot be safely converted to a numeric id with map (when it is non-numeric)" in {
      a [NumberFormatException] should be thrownBy {
        Validation.map(Success("abcd"))(_.toInt)
      }
    }

    "instead, we can apply such a conversion using a StringAsInteger validator and flatMap" - {
      /*
       * Note the support for interop with Try.
       * (Interop is also available with Either and Option).
       */
      object StringAsInteger {
        def apply(str: String): ValidationWithMessage[Int] =
          Validation.fromTry(Try(str.toInt), _ => s"String [$str] does not represent an integer")
      }

      "when numeric" in {
        Validation.flatMap(Success("1234")) {
          StringAsInteger(_)
        } shouldBe Success(1234)
      }

      "when non-numeric" in {
        Validation.flatMap(Success("abcd")) {
          StringAsInteger(_)
        } shouldBe Failure("String [abcd] does not represent an integer")
      }
    }
  }

  /*
   * Validating an individual parameter is helpful, but we need to be able to compose the result of
   * validating multiple parameters.  This is where map2, map3 ... and friends come in.
   */
  "The result of validating individual parameters can be composed" - {
    case class Params(id: String, period: YearMonth)

    "when all are valid" in {
      val validIdParam = "abcd"
      val validPeriodParam = "201803"

      Validation.map2(
        StringSatisfiesMinimumLength(4)(validIdParam),
        StringAsYearMonth(validPeriodParam)
      )(Params) shouldBe Success(Params(validIdParam, YearMonth.of(2018, MARCH)))
    }

    /*
     * Unlike a flatMap based approach where we would stop on the first Failure, map2 and friends allows us to
     * apply all of the validations and collect all of the failure messages.
     */
    "all validation messages are collected when any of the parameters are invalid" in {
      val invalidIdParam = "abc"
      val invalidPeriodParam = "42"

      Validation.map2(
        StringSatisfiesMinimumLength(4)(invalidIdParam),
        StringAsYearMonth(invalidPeriodParam)
      )(Params) shouldBe Failure(
        "String [abc] does not satisfy minimum length of [4]",
        List("Value [42] does not comply with yearMonth format [uuuuMM]"))
    }
  }
}
