package quickcheck

import org.scalacheck._

import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("empty-findMin-01") = forAll {
    e: Int =>
      val h = insert(e, empty)
      e == findMin(h)
  }

  property("meld-findMin-01") = forAll(genNonEmptyHeap, genNonEmptyHeap) {
    (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      math.min(m1, m2) == findMin(meld(h1, h2))
  }

  property("meld-findMin-02") = forAll(genNonEmptyHeap) {
    h: H =>
      val m = findMin(h)
      m == findMin(meld(h, empty)) &&
        m == findMin(meld(empty, h))
  }

  property("heap-sorted-01") = forAll {
    h: H =>
      val l = asList(h)
      l.sorted == l
  }

  property("list-mutate-01") = forAll {
    l: List[Int] =>
      val h = fromList(l)
      l.sorted == asList(h)
  }

  property("list-mutate-02") = forAll {
    (l1: List[Int], l2: List[Int]) =>
      val h1 = fromList(l1)
      val h2 = fromList(l2)
      (l1 ++ l2).sorted == asList(meld(h1, h2))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Utils
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def fromList(l: List[Int]): H = l match {
    case Nil => empty
    case x :: xs => insert(x, fromList(xs))
  }

  def asList(h: H): List[Int] = isEmpty(h) match {
    case true => Nil
    case false => findMin(h) :: asList(deleteMin(h))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Generators
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val genEmptyHeap: Gen[H] = value(empty)

  lazy val genNonEmptyHeap: Gen[H] = {
    for {
      e <- chooseNum(Int.MinValue, Int.MaxValue);
      h <- genAnyHeap
    } yield insert(e, h)
  }

  lazy val genAnyHeap: Gen[H] = oneOf(genEmptyHeap, genNonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genAnyHeap)

}
