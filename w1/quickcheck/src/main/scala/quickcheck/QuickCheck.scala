package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>

    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>

    if (a < b) {
      val h = insert(a, empty)
      findMin(h) == a
    } else if (b < a) {
      val h = insert(b, empty)
      findMin(h) == b
    } else true
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- Gen.oneOf[H](empty, genHeap)
  } yield insert(k, m)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
