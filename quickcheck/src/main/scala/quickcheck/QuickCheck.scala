package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min two values") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insert an element to empty and delete it") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("check min after delete") = forAll { (a: Int, b: Int) =>
    findMin(deleteMin(insert(a, insert(b, empty)))) == Math.max(a, b)
  }

  property("find min of two melded heap") = forAll { (a: Int, b: Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("find min of two melded heap") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == Math.min(findMin(a), findMin(b))
  }

  property("validate heap") = forAll { heap: H =>
    def getList(heap: H): List[A] =
      if (isEmpty(heap)) Nil
      else findMin(heap)::getList(deleteMin(heap))

    def validate(list: List[Int]): Boolean = list match {
      case Nil => true
      case t::Nil => true
      case a::b::rest => if (a<=b) validate(rest) else false
    }

    validate(getList(heap))
  }

  property("insert list and compare with sorted") = forAll { list: List[Int] =>
    def getList(heap: H): List[A] =
      if (isEmpty(heap)) Nil
      else findMin(heap)::getList(deleteMin(heap))

    def insertList(elementToInsert : List[Int], acc: H): H = elementToInsert match {
      case Nil => acc
      case a :: rest => insertList(rest, insert(a, acc))
    }

    list.sortWith((x, y) => x<=y) == getList(insertList(list, empty))
  }


  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
