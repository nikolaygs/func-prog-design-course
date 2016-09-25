package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min on non empty") = forAll { (a: Int, b: Int, c: Int) =>
    val min = Math.min(a, Math.min(b, c))
    val test = insert(a, insert(b, insert(c, empty)))
    findMin(test) == min
  }

  property("deleteMin") = forAll { (a: Int, b: Int, c: Int) =>
    val min = Math.min(a, Math.min(b, c))
    val test = insert(a, insert(b, insert(c, empty)))

    val expected =
      if      (min == a) insert(b, insert(c, empty))
      else if (min == b) insert(a, insert(c, empty))
      else               insert(a, insert(b, empty))

    deleteMin(test) == expected
  }

  
  property("delete all elems") = forAll { (a: Int, b: Int, c: Int) =>
    val test = insert(a+1, insert(a, insert(a+2, empty)))
    val result = deleteMin(deleteMin(deleteMin(test)))

    result == empty
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def merge(from: H, to: H): H = {
      if (isEmpty(from)) to
      else {
        val min = findMin(from)
        val remainded = merge(deleteMin(from), to)
        insert(min, remainded)
      }
    }

    def equals(first: H, second: H): Boolean = {
      if (isEmpty(first) && isEmpty(second))
        true
      else {
        val min1 = findMin(first)
        val min2 = findMin(second)
        
        (min1 == min2) && equals(deleteMin(first), deleteMin(second))
      }
    }

    val meldHeap = meld(h1, h2)
    val mergedHeap = merge(h1, h2)

    equals(meldHeap, mergedHeap)
  }

  property("meld min") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    val minMelded = findMin(melded)
    val min = Math.min(findMin(h1), findMin(h2))

    minMelded == min
  }
  
}
