package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1 (Bogus 1)") = forAll { (h: H) =>

    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }



  property("meld and find min (Bogus 5)") = {
    val a = 1
    val h = insert(a, empty)
    val b = 0
    val h2 = insert(b, empty)

    findMin(meld(h, h2)) == 0
  }

  property("insert and remove  must be empty") = forAll { (a: Int) =>

    val h = insert(a, empty)

    deleteMin(h) == empty

  }


  property("insert 3, delete and find min (Bogus 4)") = {

    val h = insert(66,insert(44,insert(55, empty)))
    findMin(deleteMin(h)) == 55
  }

  property("Sorted mins") = forAll { (list: List[Int]) =>


    var h = empty
    list.foreach(i => {
      h = insert(i, h)
    })

    val e = isEmpty(h)

    if (e) true
    else {
      val min = findMin(h)
      e || checkOrder(deleteMin(h), min, true)
    }

  }

  def checkOrder(h: H, last: Int, b: Boolean): Boolean = {

    if (isEmpty(h)) b
    else {
      val currentMin = findMin(h)
      checkOrder(deleteMin(h), currentMin, currentMin >= last & b)
    }

  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- Gen.oneOf[H](empty, genHeap)
  } yield insert(k, m)
}
