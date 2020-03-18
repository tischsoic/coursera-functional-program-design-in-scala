package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      n <- arbitrary[A]
      h <- frequency((1, const(empty)), (9, genHeap))
    } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def heapToList(h: H): List[A] =
    if (isEmpty(h)) Nil else findMin(h) :: heapToList(deleteMin(h))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin returns min of 2 values inserted into empty heap") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("Deleting last element from heap should result in empty heap") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    h == empty
  }

  property("Calling findMin and deleteMin recursively till end should return values in descending order") = forAll { (h: H) =>
    val heapList = heapToList(h) // TODO: bad idea ??? -> what if toList never ends because heap alg is broken
    heapList == heapList.sorted
  }

  property("findMin of melded 2 heaps should return min from 1st heap or 2nd heap") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  property("Inserting N elements into heap " +
    "and calling findMin;deleteMin recursively till end " +
    "should return N inserted values in descending order") = forAll { (l: List[Int]) =>
    val h = l.foldLeft(empty)((h, x) => insert(x, h))
    val heapList = heapToList(h)

    heapList == l.sorted
  }

}
