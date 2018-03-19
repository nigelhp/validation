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
