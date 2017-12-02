import Task1.{HalfwayAround, NextOne}
import org.scalatest.{FlatSpec, Matchers}


class Task1Spec extends FlatSpec with Matchers with Task1 {

  //  For example:
  //
  //  1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit
  //       and the third digit (2) matches the fourth digit.
  //  1111 produces 4 because each digit (all 1) matches the next.
  //  1234 produces 0 because no digit matches the next.
  //  91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

  "Captcha solver" should "solve captchas when comparing next digit" in {
    val table = List(
      ("1122", 3),
      ("1111", 4),
      ("1234", 0),
      ("91212129", 9)
    )

    table.foreach { case (input, expectedResult) =>
      solveCaptchaFor(input, NextOne) shouldEqual expectedResult
    }
  }

  //  For example:
  //
  //  1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
  //  1221 produces 0, because every comparison is between a 1 and a 2.
  //  123425 produces 4, because both 2s match each other, but no other digit has a match.
  //  123123 produces 12.
  //  12131415 produces 4.

  it should "solve captchas when comparing halfway around the list" in {
    val table = List(
      ("1212", 6),
      ("1221", 0),
      ("123425", 4),
      ("123123", 12),
      ("12131415", 4)
    )

    table.foreach { case (input, expectedResult) =>
      solveCaptchaFor(input, HalfwayAround) shouldEqual expectedResult
    }
  }

}
