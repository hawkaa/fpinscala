import org.scalatest.{FlatSpec, Matchers}

class Chp2Test extends FlatSpec with Matchers {
  "isSorted" should "return true on ordered" in {
    Chp2.isSorted(Array(1, 5, 7), (a: Int, b: Int) => a <= b) should be (true)
    Chp2.isSorted(Array(1, 1, 1), (a: Int, b: Int) => a <= b) should be (true)
  }

  "isSorted" should "return false on unordered" in {
    Chp2.isSorted(Array(1, 1, 0), (a: Int, b: Int) => a <= b) should be (false)
    Chp2.isSorted(Array(9, 2, 1), (a: Int, b: Int) => a <= b) should be (false)
    Chp2.isSorted(Array(9, 2, 5), (a: Int, b: Int) => a <= b) should be (false)

  }

  "isSorted" should "handle edge cases" in {
    Chp2.isSorted(Array(1), (a: Int, b: Int) => a <= b) should be (true)
    Chp2.isSorted(Array(), (a: Int, b: Int) => a <= b) should be (true)
  }

  "curry" should "do this" in {
    val tete = (a: Double, b: Int) => (a * b).toString
    val curried = Chp2.curry(tete)
    tete(5, 6) should be (curried(5)(6))
  }

  "compose" should "do this" in {
    val composed = Chp2.compose((b: Int) => b*2, (a: Int) => a + 2)
    composed(5) should be (14)
  }
}