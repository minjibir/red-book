package ds

import org.scalatest.wordspec.AnyWordSpec
import scala.collection.View

class JistSpec extends AnyWordSpec {

  // val jistInt: Jist[Int]    = Jist(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
  // val jistStr: Jist[String] = Jist("a", "b", "c", "d", "e", "f")

  // val str0jist: Jist[String] = Jist("a", "b", "c")
  // val str1jist: Jist[String] = Jist("d", "e", "f")

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

    "when both Jist are non empty" should {
      "return the two merged into one Jist" in {
        val str0jist  = Jist("a", "b", "c")
        val str1jist  = Jist("d", "e", "f")
        val actual    = Jist("a", "b", "c", "d", "e", "f")

        val expected  = Jist.concat(str0jist, str1jist)

        assert(expected == actual)
      }
    }
  }

  "map function" should {
    "correctly add one to each function to the Jist" in {
      val jist = Jist(0, 1, 2, 3, 4, 5)
      val expected = Jist(1, 2, 3, 4, 5, 6)
      val actual = Jist.map(jist)(_ + 1)
      
      assert(actual == expected)
    }

    "correctly multiply each element by 2" in {
      val jist = Jist(0, 1, 2, 3, 4, 5)
      val expected = Jist(0, 2, 4, 6, 8, 10)
      val actual = Jist.map(jist)(_ * 2)
      
      assert(actual == expected)
    }
  }

  "filter function" should {
    "filter out the ints that doesn't matches the given cond" in {
      val jist = Jist(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val expected = Jist(1, 3, 5, 7, 9)
      val actual = Jist.filter(jist)(_ % 2 > 0)
      
      assert(actual == expected)
    }
  
    "filter out the strings that doesn't matches the given cond" in {
      val jist = Jist("a", "b", "c", "d", "e")
      val expected = Jist("b", "c", "d", "e")
      val actual = Jist.filter(jist)(_ != "a")
      
      assert(actual == expected)
    }
  }

  "flatMap function called with multiple Jist" should {
    "return a single list containing all the sub-list's elems" in {      
      val expected = Jist(1,1,2,2,3,3)
      
      val actual = Jist.flatMap(Jist(1,2,3))(i => Jist(i,i))
      
      assert(actual == expected)
    }
  }

  "combine function given two Jists " should {
    "return one Jist by adding corresponding elems" in {
      val a = Jist(1, 2, 3)
      val b = Jist(4, 5, 6)
      val expected = Jist(5, 7, 9)
      val actual   = Jist.zipWith(a, b)(_ + _)

      assert(actual == expected)
    }
  }
  
}
