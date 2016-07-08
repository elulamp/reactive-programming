package suggestions


import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import rx.lang.scala._
import suggestions.gui._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite with Matchers with ScalaFutures {

  object mockApi extends WikipediaApi {

    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }

    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable.just("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("timedOut() should behave as it suppose") {
    val observable = Observable.just(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L)

    val sum = observable.foldLeft(0) { (accumulator, value) =>
      accumulator + value._1
    }

    val p = Promise[Int]()
    sum.subscribe(event => p.success(event))

    whenReady(p.future, timeout(2 seconds)) {
      sum => sum should be(1)
    }
  }

  test("any exceptions in the Observable should be wrapped into Failure objects") {
    val ex = new Exception("foo")

    val o = Observable.just(1, 2, 3).map {
      v => if (v % 2 == 0) throw ex else v
    }

    val ro = o.recovered

    var completed = false

    val observed = mutable.Buffer[Try[Int]]()

    ro subscribe(
      v => observed += v,
      t => fail(s"stream error $t"),
      () => completed = true)

    assert(completed)
    observed should contain theSameElementsAs Seq(Success(1), Failure(ex))
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable.just(1, 2, 3)
    val remoteComputation = (n: Int) => Observable.just(0 to n : _*)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("WikipediaApi should correctly use concatRecovered from spec") {
    val requests = Observable.just(1, 2, 3, 4, 5)

    val ex = new Exception

    val rf = { num: Int => if (num != 4) Observable.just(num) else Observable.error(ex) }

    val responses = requests concatRecovered rf

    val items = responses.foldLeft(Seq.empty[Try[Int]]) { (acc, tn) => tn +: acc }

    items subscribe {
      observed => observed should contain theSameElementsAs Seq(Success(1), Success(2), Success(3), Failure(ex), Success(5))
    }
  }


}
