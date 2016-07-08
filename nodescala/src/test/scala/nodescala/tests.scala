package nodescala

import nodescala.NodeScala._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import org.scalatest.time._

import scala.collection._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite with ScalaFutures with Matchers {

  test("A Future should always be completed") {

    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be completed") {

    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      fail()
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("All futures should complete") {

    val fs = List(Future{1}, Future{2}, Future{3})

    val f = Future.all(fs)

    whenReady(f, timeout(Span(1, Nanosecond))) { vs =>
      vs should contain theSameElementsAs List(1,2,3)
    }
  }

  test("Should complete with the value of the first future that completes") {

    val any = Future.any(List(Future.never, Future.successful(2), Future.never))

    whenReady(any) { v =>
      v should be(2)
    }
  }

  test("Should complete future after delay") {

    val delayed = Future.delay(100 millis)

    delayed should not be 'completed

    whenReady(delayed, timeout(Span(100, Milliseconds))) { _ =>
      delayed shouldBe 'completed
    }
  }

  test("Should fail if now called on non completed future") {

    val nonComplitingFuture = Future.never

    a [NoSuchElementException] should be thrownBy nonComplitingFuture.now
  }

  test("Should return value if now is called on completed future") {

    val f = Future.always(12)
    f.now shouldBe 12
  }

  test("Should continue future with") {
    val f = Future {Thread.sleep(100); 12}

    val f2 = f.continueWith(f => f.now + 12)

    whenReady(f2) { v =>
      v shouldBe 24
    }
  }

  test("Should continue future") {
    val f = Future {Thread.sleep(100); 12}

    val f2 = f.continue(t => t.get + 12)

    whenReady(f2) { v =>
      v shouldBe 24
    }
  }

  test("should run and be cancelled") {

    val promise = Promise[Duration]()
    val start = Duration(System.nanoTime(), NANOSECONDS)

    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          println("working")
        }
        println("done")
        promise.success(Duration(System.nanoTime(), NANOSECONDS))
      }
    }

    val f = Future.delay(100 millis)

    f onSuccess {
      case _ => working.unsubscribe()
    }

    whenReady(promise.future, timeout(Span(150, Millis))) { stop =>
      val elapsed = stop - start
      val beWithinTolerance = be >= 100L and be < 120L
      elapsed.toMillis should beWithinTolerance
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String): Unit = {
      response += s
    }
    def close(): Unit = {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request): Unit = {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("Server should be stoppable if receives infinite  response") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => Iterator.continually("a")
    }

    // wait until server is really installed
    Thread.sleep(500)

    val webpage = dummy.emit("/testDir", Map("Any" -> List("thing")))
    try {
      // let's wait some time
      Await.result(webpage.loaded.future, 1 second)
      fail("infinite response ended")
    } catch {
      case e: TimeoutException =>
    }

    // stop everything
    dummySubscription.unsubscribe()
    Thread.sleep(500)
    webpage.loaded.future.now // should not get NoSuchElementException
  }
}




