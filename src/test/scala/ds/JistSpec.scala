package ds

import org.scalatest.wordspec.AnyWordSpec

class JistSpec extends AnyWordSpec {
  
	val jistInt: Jist[Int]    = Jist(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
	val jistStr: Jist[String] = Jist("a", "b", "c", "d", "e", "f")

	val str0jist: Jist[String] = Jist("a", "b", "c")
	val str1jist: Jist[String] = Jist("d", "e", "f")

  "tail function" when {
    "called on an EmptyJist" must {
      "return an EmptyJist" in {
        val emptyJist = EmptyJist
        val expected  = Jist.tail(emptyJist)

        assert(expected == EmptyJist)
      }
    }

    "called on List with 1 element" must {
      "return EmptyJist" in {
        val jist      = Cons(1, EmptyJist)
        val expected  = Jist.tail(jist)

        assert(expected == EmptyJist)
      }
    }

    "called on list with multiple non-emtpy Jist" should {
      "return all but the first element" in {
        val expected = Jist.tail(jistStr)
        
        assert(expected == Jist("b", "c", "d", "e", "f"))
      }
    }
  }

  "setHead function" when {
    "called on EmptyJist" should {
      "return a new Jist with one element" in {
        val expected  = Jist.setHead(EmptyJist, 0)
        assert(expected == Cons(0, EmptyJist))
      }
    }

    "called on Jist with 1 elelment" should {
      "return Jist with two element" in {
        val expected  = Jist.setHead(Cons("z", EmptyJist), "y")

        assert(expected == Cons("y", Cons("z", EmptyJist)))
      }
    }
  }
}
