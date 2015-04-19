package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // =============== Generators =======================
  /**
   * Generator for a constant empty heap
   */
  val genEmptyHeap: Gen[H] = const(empty)
  /**
   * Generator for heap, made lazy to prevent
   * infinite recursion
   */
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    m <- oneOf(genEmptyHeap, genHeap)
  } yield insert(i, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  val genNonEmptyHeap: Gen[H] = genHeap suchThat { !isEmpty(_) }
    
  // helper generator for testing
  def genNonEmptyHeap(sz: Int = 5): Gen[H] = {
    require(sz >= 0, "Size of non empty heap can not be less than zero")
    println("Size -> " + sz);
    genHeap suchThat (HeapToListConvertor(_).length > sz)
  }

  // =============== Properties =======================
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    (a != b) ==> {
      val h = insert(b, insert(a, empty))
      if (a < b) findMin(h) ?= a else findMin(h) ?= b
    }
  }

  property("empty.insert.delete.empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("delete.length.reduction") = forAll { h: H =>
    !isEmpty(h) ==> {
      val orig_len = HeapToListConvertor(h).length
      val len_after_del = HeapToListConvertor(deleteMin(h)).length
      orig_len - 1 ?= len_after_del
    }
  }

  property("verify.sorted.elems") = forAll { h: H =>
    (!isEmpty(h)) ==> {
      val list = HeapToListConvertor(h)
      list.sorted ?= list
    }
  }
  
  
  property("verify.melding.sorted.elems") = forAll { (h1: H, h2: H) =>
    (! (isEmpty(h1) || isEmpty(h2))) ==> {
      val list = HeapToListConvertor(meld(h1, h2))
      ( HeapToListConvertor(h1) ++ HeapToListConvertor(h2)).sorted ?= list
    }
  }

  property("melding.min") = forAll { (h1: H, h2: H) =>
    !(isEmpty(h1) || isEmpty(h2)) ==> {
      val mH = meld(h1, h2);
      // TODO: in a real app, ideally minimums should be extracted
      // only once and then evaluated in expression!
      (findMin(mH) ?= findMin(h1)) || (findMin(mH) ?= findMin(h2))
    }
  }

  property("emptyHeap.findMin.exception") = forAll(genEmptyHeap) { h: H =>
    Prop.throws(classOf[NoSuchElementException]) { findMin(h) }
  }

  // ================== Helper Functions =======================
  def HeapToListConvertor(inHeap: H): List[Int] =
    if (isEmpty(inHeap)) Nil else findMin(inHeap) :: HeapToListConvertor(deleteMin(inHeap))

}
