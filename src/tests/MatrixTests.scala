package tests

import linear_programming.GenericMatrix

/**
 * Created by weijiayi on 3/24/15.
 */
class MatrixTests extends MyTest{
  "matrix should have right size" in {
    intercept[IllegalArgumentException]{
      val elements=Array(Array(1,2,3),Array(1,2))
      new GenericMatrix(elements)
    }
  }
  val matrix33= new GenericMatrix[Int](Array(Array(1,2,3),Array(4,5,6),Array(7,8,9)))
  "matrix33" should {
    "tell you its size" in {
      matrix33.rowNumber shouldBe 3
      matrix33.colNumber shouldBe 3
    }
    "give you right elements" in {
      matrix33(0,0) shouldBe 1
      matrix33(1,1) shouldBe 5
      matrix33(1,2) shouldBe 6
      intercept[IndexOutOfBoundsException]{
        matrix33(4,4)
      }
    }
  }
}
