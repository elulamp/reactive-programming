package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with Matchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {

    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {

    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeDelta") {

    val result = Polynomial.computeDelta(Var(2), Var(5), Var(-3))
    assert(result() == 49)
  }

  test("computeSolutions has two solutions") {

    val result = Polynomial.computeSolutions(Var(2), Var(5), Var(-3), Var(49))

    result() should contain theSameElementsAs Set(-3, 0.5)
  }

  test("computeSolutions has one solution") {

    val result = Polynomial.computeSolutions(Var(1), Var(6), Var(-9), Var(0))

    result() should contain theSameElementsAs Set(-3)
  }

  test("computeSolutions no solutions") {

    val result = Polynomial.computeSolutions(Var(2), Var(5), Var(-3), Var(-49))

    result() should contain theSameElementsAs Set()
  }

  test("should calculate single literal") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map("a" -> Signal(Literal(0.1))))

    assert(values("a")() == 0.1)
  }

  test("should calculate single reference") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Literal(0.1)),
      "b" -> Signal(Ref("a"))))

    assert(values("b")() == 0.1)
  }

  test("should detect single cycle") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Ref("a"))
    ))
    assert(values("a")().isNaN)
  }

  test("should detect multiple cycles") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Ref("c")),
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Ref("b"))
    ))
    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
    assert(values("c")().isNaN)
  }

  test("should detect multiple cycles2") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Plus(Literal(1), Ref("a"))),
      "b" -> Signal(Ref("a"))
    ))
    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
  }

  test("should detect multiple cycles3") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Ref("a"))
    ))
    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
    assert(values("c")().isNaN)
  }

  test("should detect multiple cycle with calculation") {

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> Signal(Plus(Ref("b"),Ref("c"))),
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Ref("b"))
    ))
    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
    assert(values("c")().isNaN)
  }

  test("should update on change") {

    val a: Var[Expr] = Var(Literal(1.0))

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> a,
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Ref("b"))
    ))
    assert(values("c")() == 1.0)

    a.update(Literal(2.0))

    assert(values("c")() == 2.0)
  }

  test("should update all to NaN on cycle change") {

    val a: Var[Expr] = Var(Literal(1.0))

    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      "a" -> a,
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Ref("b"))
    ))
    assert(values("c")() == 1.0)

    a.update(Ref("c"))

    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
    assert(values("c")().isNaN)
  }

}
