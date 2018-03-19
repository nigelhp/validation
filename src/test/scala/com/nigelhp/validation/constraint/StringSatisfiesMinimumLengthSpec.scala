package com.nigelhp.validation.constraint

import com.nigelhp.validation.{Failure, Success}
import org.scalatest.{FreeSpec, Matchers}

class StringSatisfiesMinimumLengthSpec extends FreeSpec with Matchers {
  "A string" - {
    "is invalid" - {
      "when it has a length less than the minimum" in {
        StringSatisfiesMinimumLength(4)("abc") shouldBe Failure("String [abc] does not satisfy minimum length of [4]")
      }
    }

    "is valid" - {
      "when it has a length equal to the minimum" in {
        StringSatisfiesMinimumLength(4)("abcd") shouldBe Success("abcd")
      }

      "when it has a length longer than the minimum" in {
        StringSatisfiesMinimumLength(4)("abcde") shouldBe Success("abcde")
      }
    }
  }
}
