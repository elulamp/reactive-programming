package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    property("min1") = forAll { a: Int =>
        val h = insert(a, empty)
        findMin(h) == a
    }

    property("gen1") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    }

    property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (a: Int, b: Int) =>
        val h = insert(a, insert(b, empty))
        findMin(h) == Math.min(a, b)
    }

    property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll { a: Int =>
        val h = insert(a, empty)
        isEmpty(deleteMin(h))
    }


    property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll { (h1: H, h2: H) =>

        val m1 = if (isEmpty(h1)) 0 else findMin(h1)
        val m2 = if (isEmpty(h2)) 0 else findMin(h2)

        val union = meld(h1, h2)

        val um = if (isEmpty(union)) 0 else findMin(union)

        um == m1 || um == m2

    }

    property("all inserted elements in non empty heap should be in order") = forAll { ys: List[A] =>

        @tailrec
        def createHeap(xs: List[A], h: H): H = {

            xs match {
                case Nil => h
                case head :: tail => createHeap(tail, insert(head, h))
            }
        }

        val h = createHeap(ys, empty)

        @tailrec
        def loop(h: H, elements: List[A]): List[A] = {

            if (isEmpty(h)) {
                elements
            } else {
                val min = findMin(h)
                loop(deleteMin(h), elements :+ min)
            }
        }

        val elements = loop(h, Nil)

        elements == ys.sorted
    }

    private def emptyHeap: Gen[H] = {

        const(empty)
    }

    private def nonEmptyHeap: Gen[H] = for {
        v <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)

    lazy val genHeap: Gen[H] = for {
        isEmpty <- oneOf(true, false)
        h <- if (isEmpty) emptyHeap else nonEmptyHeap
    } yield h

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
