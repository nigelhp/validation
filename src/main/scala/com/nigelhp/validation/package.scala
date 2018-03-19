package com.nigelhp

package object validation {
  /**
    * Standardise on a simple validation error message as our representation of the Failure type
    *
    * @tparam A the type of the Success value
    */
  type ValidationWithMessage[+A] = Validation[String, A]
}
