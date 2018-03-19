package com.nigelhp.validation.string

import com.nigelhp.validation.{Failure, Success, ValidationWithMessage}

object SatisfiesMinimumLength {
  def apply(minLength: Int)(value: String): ValidationWithMessage[String] =
    if (value.length < minLength) Failure(s"String [$value] does not satisfy minimum length of [$minLength]")
    else Success(value)
}
