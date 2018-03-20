### Validation

#### A Validation Type
An algebraic data type is defined for Validation, that has two cases -
`Success` and `Failure`.  This is similar to `Either` and its cases
`Left` and `Right`.  The difference is that the Failure case can contain
a List of values rather than just a single value.  This will be useful
when we compose validations (see Composing Multiple Validators).

    sealed trait Validation[+E, +A]

    case class Success[A](a: A) extends Validation[Nothing, A]
    case class Failure[E](head: E, tail: List[E] = Nil) extends Validation[E, Nothing]


While Validation supports type parameters for both the Failure value and
the Success value, we standardise on String as our representation for
Failure.  This will represent a validation failure message.

    type ValidationWithMessage[+A] = Validation[String, A]


#### Validators
A validator is simply a function that takes an argument, and returns a
Validation.  These have a single responsibility, and can be tested
independently.

As an example:

    object StringSatisfiesMinimumLength {
      def apply(minLength: Int)(value: String): ValidationWithMessage[String] =
        if (value.length < minLength) Failure(s"String [$value] does not satisfy minimum length of [$minLength]")
        else Success(value)
    }

This project contains two examples:
* StringSatisfiesMinimumLength (shown above)
* StringAsYearMonth

Note that there seem to be two types of validations.  Some are _constraints_
(such as string length, integer range etc.) where the argument is returned
unchanged when valid.  Others are _conversions_ (such as parsing strings
that should represent a date), where the most convenient approach to
determining whether the string is valid is to attempt to parse it, catching
any errors.  In this example, a modified representation is returned (eg.
a `Date` object) if the input is valid, rather than the original input.


#### Working with a Validation
Assume that we are using the "tiny type" pattern, and have an Id wrapper
for string.  _map_ allows us to convert a valid string into a valid Id.

    case class Id(value: String)

    Validation.map(StringSatisfiesMinimumLength(4)("abcd  ".trim))(Id) shouldBe Success(Id("abcd"))

This is "safe" because the operation cannot throw an exception.

Now assume that an Id must satisfy a minimum length, __and__ be numeric.
This is where _flatMap_ comes in, which allows us to apply a validation
to the result of a prior validation.

    Validation.flatMap(StringSatisfiesMinimumLength(4)("1234  ".trim)) {
      StringAsInteger(_)
    } shouldBe Success(1234)

Note that _map_ and _flatMap_ will not apply the function to a Failure,
and simply return the existing Failure unmodified.


#### Composing Multiple Validators
While _flatMap_ can be used to compose validators, as highlighted above
it will stop at the first Failure.  As with the validation of user input
on web forms, it may be preferable to ensure that all parameters are
fully validated, so that a comprehensive list of problems can be
returned in a single response.  This is where _map2_, _map3_, ... mapN
and friends come in.

The idea here is to only apply the supplied functions if all validations
succeed, but to accumulate all failures in the event of failure.

A typical example is for the function to apply to be a case class
constructor.  The following poplulates an instance of `Params` only if
all of its constituents are considered valid.

    case class Params(id: String, period: YearMonth)

    Validation.map2(
      StringSatisfiesMinimumLength(4)("abcd"),
      StringAsYearMonth("201803")
    )(Params) shouldBe Success(Params("abcd", YearMonth.of(2018, MARCH)))


#### Interoperability
##### Try
There is support for creating a validation from a `Try`.  For example:

    object StringAsInteger {
      def apply(str: String): ValidationWithMessage[Int] =
        Validation.fromTry(Try(str.toInt), _ => s"String [$str] does not represent an integer")
    }

In this example, we use a variant that converts the `Throwable` case
into a string to satisfy the type of `ValidationWithMessage`.  Otherwise,
the result would be a `Validation[Throwable, A]`.

##### Either
There is support for converting to/from an `Either`.

When converting from an Either, the assumption is that the Success case
is on the Right.  It is also assumed that the Either represents a single
validation, and so a Left will map to a Failure that contains a single
failure message.

When converting to an Either, we must take account of the fact that a
Failure may contain many failure messages.  We therefore map a Failure
to a Left\[List\[E\]\].

##### Option
There is support for converting to an `Option`.  A Success will create a
Some, a Failure a None.


#### Examples
See `com.nigelhp.validation.WorkedExampleSpec` for an executable
version of some of the examples outlined above.
