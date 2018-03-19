package com.nigelhp.validation.constraint

import com.nigelhp.validation.{Failure, Success, ValidationWithMessage}

object StringSatisfiesMinimumLength {
  def apply(minLength: Int)(value: String): ValidationWithMessage[String] =
    if (value.length < minLength) Failure(s"String [$value] does not satisfy minimum length of [$minLength]")
    else Success(value)
}
