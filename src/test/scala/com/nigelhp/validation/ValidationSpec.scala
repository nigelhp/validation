package com.nigelhp.validation

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class ValidationSpec extends FreeSpec with Matchers with MockFactory {
  private trait FoldFixture {
    val onSuccess = mockFunction[Int, Int]
    val onFailure = mockFunction[List[String], Int]
    val foldResult = 666
  }

  private trait MapFixture {
    val fn = mockFunction[Int, Int]
  }

  private trait FlatMapFixture {
    val fn = mockFunction[String, Validation[String, Int]]
  }

  private trait Map2Fixture {
    val intValue = 42
    val longValue = 666L
    val fn = mockFunction[Int, Long, String]
  }

  "fold" - {
    "applies the onFailure function to a Failure" - {
      "when the Failure contains a singleton value" in new FoldFixture {
        val failureMsg = "validation failed"
        onFailure.expects(failureMsg :: Nil).returning(foldResult)

        Validation.fold[String, Int, Int](onFailure, onSuccess)(Failure(failureMsg)) shouldBe foldResult
      }

      "when the Failure contains multiple values" in new FoldFixture {
        val headFailure = "validation failed"
        val tailFailures = List("this failed", "that failed")
        onFailure.expects(headFailure :: tailFailures).returning(foldResult)

        Validation.fold[String, Int, Int](onFailure, onSuccess)(Failure(headFailure, tailFailures)) shouldBe foldResult
      }
    }

    "applies the onSuccess function to a Success" in new FoldFixture {
      val successValue = 42
      onSuccess.expects(successValue).returning(foldResult)

      Validation.fold[String, Int, Int](onFailure, onSuccess)(Success(successValue)) shouldBe foldResult
    }
  }

  "map" - {
    "applies the supplied function to a Success" in new MapFixture {
      val inputValue = 42
      val outputValue = 666
      fn.expects(inputValue).returning(outputValue)

      Validation.map(Success(inputValue))(fn) shouldBe Success(outputValue)
    }

    "does not apply the supplied function to a Failure" in new MapFixture {
      Validation.map(Failure("failure message"))(fn) shouldBe Failure("failure message")
    }
  }

  "flatMap" - {
    "applies the supplied function to a Success" in new FlatMapFixture {
      val inputValue = "input"
      val outputValue = Success(42)
      fn.expects(inputValue).returning(outputValue)

      Validation.flatMap(Success(inputValue))(fn) shouldBe outputValue
    }

    "does not apply the supplied function to a Failure" in new FlatMapFixture {
      Validation.flatMap(Failure("failure message"))(fn) shouldBe Failure("failure message")
    }
  }

  "map2" -{
    "applies the supplied function when both validations are Successes" in new Map2Fixture {
      val stringValue = "some-string"
      fn.expects(intValue, longValue).returning(stringValue)

      Validation.map2(Success(intValue), Success(longValue))(fn) shouldBe Success(stringValue)
    }

    "captures all failure messages when both validations are Failures" in new Map2Fixture {
      Validation.map2(Failure("a failed"), Failure("b failed"))(fn) shouldBe Failure("a failed", List("b failed"))
    }

    "returns Failure when the first validation is a Failure even though the second was a Success" in new Map2Fixture {
      Validation.map2(Failure("a failed"), Success(longValue))(fn) shouldBe Failure("a failed")
    }

    "returns Failure when the second validation is a Failure even though the first was a Success" in new Map2Fixture {
      Validation.map2(Success(intValue), Failure("b failed"))(fn) shouldBe Failure("b failed")
    }
  }

  "fromTry" - {
    "converts a scala.util.Failure to a Failure[Throwable, A] when no failure conversion function is supplied" in {
      val cause = new Exception("failure message")
      Validation.fromTry(scala.util.Failure(cause)) shouldBe Failure(cause)
    }

    "converts a scala.util.Failure to a Failure[E, A] when a failure conversion function (Throwable => E) is supplied" in {
      val cause = new Exception("failure message")
      Validation.fromTry(scala.util.Failure(cause), _.getMessage) shouldBe Failure("failure message")
    }

    "converts a scala.util.Success to a Success" in {
      val successValue = 42
      Validation.fromTry(scala.util.Success(successValue)) shouldBe Success(successValue)
    }
  }
}
