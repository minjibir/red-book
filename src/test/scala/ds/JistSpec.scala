package ds

import org.scalatest.wordspec.AnyWordSpec
import scala.collection.View

class JistSpec extends AnyWordSpec {

  "tail function" when {
    "called on an EmptyJist" must {
      "return an EmptyJist" in {
        val emptyJist = EmptyJist
        val expected = Jist.tail(emptyJist)

        assert(expected == EmptyJist)
      }
    }

    "called on List with 1 element" must {
      "return EmptyJist" in {
        val jist = Cons(1, EmptyJist)
        val expected = Jist.tail(jist)

        assert(expected == EmptyJist)
      }
    }

    "called on list with multiple non-emtpy Jist" should {
      "return all but the first element" in {
        val jistStr = Jist("a", "b", "c", "d", "e", "f")
        val expected = Jist.tail(jistStr)

        assert(expected == Jist("b", "c", "d", "e", "f"))
      }
    }
  }

  "setHead function" when {
    "called on EmptyJist" should {
      "return a new Jist with one element" in {
        val expected = Jist.setHead(EmptyJist, "a")
        assert(expected == Cons("a", EmptyJist))
      }
    }

    "called on Jist with 1 elelment" should {
      "return Jist with two element" in {
        val jistStrv = Cons("z", EmptyJist)
        val expected = Jist.setHead(jistStrv, "y")

        assert(expected == Cons("y", EmptyJist))
      }
    }

    "called on Jist with multiple elements" should {
      "return the same list of string with the new head replaced" in {
        val jistInt = Jist(9, 1, 2, 3)
        val expectedInt = Jist.setHead(jistInt, 0)

        assert(expectedInt == Jist(0, 1, 2, 3))
      }

      "return the same list of Int with the new head replaced" in {
        val jistStr = Jist("*", "b", "c")
        val expectedStr = Jist.setHead(jistStr, "a")

        assert(expectedStr == Jist("a", "b", "c"))
      }
    }

  }

  "append function" when {
    "called with an empty Jist and an elemen" should {
      "return a Jist with one element" in {
        val expected = Jist.append(1, EmptyJist)

        assert(expected == Jist(1))
      }
    }

    "called with on Jist with multiple element" should {
      "return a Jist with the new elelemtn at the end" in {
        val jistInt = Jist(1, 2, 3)
        val expectedInt = Jist.append(4, jistInt)

        assert(expectedInt == Jist(1, 2, 3, 4))
      }

      "return the same list of Int with the new head replaced" in {
        val jistStr = Jist("a", "b", "c")
        val expectedStr = Jist.append("d", jistStr)

        assert(expectedStr == Jist("a", "b", "c", "d"))
      }
    }
  }

  "concat function" when {
    "both Jist are empty" should {
      "return empty Jist" in {
        val expected = Jist.concat(EmptyJist, EmptyJist)
        assert(expected == EmptyJist)
      }
    }

    "one of the Jist is empty" should {
      "return the non empty Jist" in {
        val str0jist = Jist.concat(EmptyJist, Jist("a", "b", "c"))
        val str1jist = Jist.concat(Jist("d", "e", "f"), EmptyJist)

        assert(str0jist == Jist("a", "b", "c"))
        assert(str1jist == Jist("d", "e", "f"))
      }
    }

    "both Jist are non empty" should {
      "return the two merged into one Jist" in {
        val str0jist  = Jist("a", "b", "c")
        val str1jist  = Jist("d", "e", "f")
        val actual    = Jist("a", "b", "c", "d", "e", "f")

        val expected  = Jist.concat(str0jist, str1jist)

        assert(expected == actual)
      }
    }
  }
  
  "drop function" when {
    "given an empty Jist" must {
      "return an empty Jist" in {
        val decker: Jist[String] = Jist()
        val result: Jist[String] = Jist.drop(decker, 1)

        assert(result.equals(EmptyJist))
      }
    }

    "given a Jist with single element" must {
      "return empty Jist" in {
        val decker: Jist[Int] = Cons(1, EmptyJist)
        val result: Jist[Int] = Jist.drop(decker, 1)

        assert(result.equals(EmptyJist))
      }
    }

    "given a Jist with multiple elements" should {
      "return all but the specified number" in {
        val decker: Jist[Int] = Jist(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
        val result: Jist[Int] = Jist.drop(decker, 3)

        assert(result.equals(Jist(4, 5, 6, 7, 8, 9, 0)))
      }
    }
  }

  "dropWhile function" when {
    "given an empty Jist" must {
      "return an empty Jist" in {
        val decker: Jist[Int] = Jist()
        val result: Jist[Int] = Jist.dropWhile(decker)(_ < 4)

        assert(result.equals(EmptyJist))
      }
    }

    "given a Jist with single element" must {
      "return empty Jist" in {
        val decker: Jist[String] = Jist("string")
        val result: Jist[String] = Jist.dropWhile(decker)(_.contains("s"))

        assert(result.equals(EmptyJist))
      }
    }

    "given a Jist with multiple elements" should {
      "return all but the specified number" in {
        val decker: Jist[Int] = Jist(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        val result0: Jist[Int] = Jist.dropWhile(decker)(i => i < 5)
        val result1: Jist[Int] = Jist.dropWhile(decker)(_ < 0)
        val result2: Jist[Int] = Jist.dropWhile(decker)(_ < 9)
        
        assert(result0 equals Jist(5, 6, 7, 8, 9))
        assert(result1 equals decker)
        assert(result2 equals Jist(9))
        assert(result2 equals Cons(9, EmptyJist))
      }
    }
  }

  "init function" when {
    "called with empty Jist argument" should {
      "return an empty Jist" in {
        val expected: Jist[String] = Jist.init(EmptyJist)
        
        assert(expected equals EmptyJist)
      }
    }

    "called with single element Jist" should {
      "return empty jist" in {
        val expected: Jist[Int] = Jist.init(Jist(1))

        assert(expected equals EmptyJist)
      }
    }

    "called with multiple element Jist" should {
      "return empty jist" in {
        val expected0: Jist[Int] = Jist.init(Jist(0, 1))
        val expected1: Jist[Int] = Jist.init(Jist(0, 1, 2, 3, 4))

        assert(expected0 equals Jist(0))
        assert(expected0 equals Cons(0, EmptyJist))
        assert(expected1 equals Jist(0, 1, 2, 3))
      }
    }

  }
}    
