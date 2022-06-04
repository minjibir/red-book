package ds

object Jist {

  def apply[A](a: A*): Jist[A] =
    if (a.isEmpty) EmptyJist
    else Cons(a.head, apply(a.tail: _*))

  def tail[A](list: Jist[A]): Jist[A] = list match {
    case EmptyJist     => EmptyJist
    case Cons(_, tail) => tail
  }

  def setHead[A](list: Jist[A], head: A): Jist[A] = list match {
    case EmptyJist   => Cons(head, EmptyJist)
    case Cons(_, xs) => Cons(head, xs)
  }

  def sum(jist: Jist[Int]): Int =
    foldLeft(jist, 0)(_ + _)

  def product(jist: Jist[Double]): Double =
    foldLeft(jist, 1.0)(_ * _)

  def append[A](a: A, list: Jist[A]): Jist[A] =
		/*
		foldLeft(list, list)(Cons(append(a)))
		*/
	list match {
    case EmptyJist   => Cons(a, EmptyJist)
    case Cons(x, xs) => Cons(x, append(a, xs))
  }

  def concat[A](a: Jist[A], b: Jist[A]): Jist[A] = (a, b) match {
    case (EmptyJist, EmptyJist)    => EmptyJist
    case (Cons(_, _), EmptyJist)   => a
    case (EmptyJist, Cons(_, _))   => b
    case (Cons(x, xs), Cons(_, _)) => Cons(x, concat(xs, b))
  }

  @annotation.tailrec
  def drop[A](l: Jist[A], n: Int): Jist[A] = {
    if (l == EmptyJist || n <= 0) l
    else drop(tail(l), n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: Jist[A])(f: A => Boolean): Jist[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _                  => l
  }

  def init[Int](list: Jist[Int]): Jist[Int] = list match {
    case EmptyJist                   => EmptyJist
    case Cons(_, EmptyJist)          => EmptyJist
    case Cons(x, Cons(_, EmptyJist)) => Cons(x, EmptyJist)
    case Cons(x, Cons(y, ys))        => Cons(x, Cons(y, init(ys)))
  }

  def length[A](jist: Jist[A]): Int =
    foldRight(jist, 0)((_, len) => len + 1)

  @annotation.tailrec
  def foldLeft[A, B](jist: Jist[A], b: B)(f: (A, B) => B): B =
    jist match {
      case EmptyJist  => b
      case Cons(h, t) => foldLeft(t, f(h, b))(f)
    }

  def foldRight[A, B](jist: Jist[A], b: B)(f: (A, B) => B): B =
    foldLeft(jist, b)(f)

  def reverse[A](jist: Jist[A]): Jist[A] =
    foldLeft(jist, EmptyJist: Jist[A])(Cons(_, _))

  def map[A, B](as: Jist[A])(f: A => B): Jist[B] = {

    @annotation.tailrec
    def loop(a: Jist[A], d: Jist[B])(f: A => B): Jist[B] =
      a match {
        case EmptyJist  => d
        case Cons(h, t) => loop(t, append(f(h), d))(f)
      }

    loop(as, EmptyJist)(f)
  }

  def filter[A](as: Jist[A])(f: A => Boolean): Jist[A] = {

    @annotation.tailrec
    def loop(a: Jist[A], b: Jist[A])(f: A => Boolean): Jist[A] = a match {
      case EmptyJist => b
      case Cons(h, t) =>
        val acc: Jist[A] = if (f(h)) append(h, b) else b
        loop(t, acc)(f)
    }

    loop(as, EmptyJist)(f)
  }

  def flatMap[A, B](as: Jist[A])(f: A => Jist[B]): Jist[B] = {

    @annotation.tailrec
    def loop(a: Jist[A], b: Jist[B])(f: A => Jist[B]): Jist[B] =
      a match {
        case EmptyJist  => b
        case Cons(h, t) => loop(t, concat(b, f(h)))(f)
      }

    loop(as, EmptyJist)(f)
  }

  // def flatFilter[A](as: Jist[A])(f: A => Boolean): Jist[A] =
  // 	flatMap(as)(f)

  def zipWith[A](a: Jist[A], b: Jist[A])(f: (A, A) => A): Jist[A] = {
    def loop(x: Jist[A], y: Jist[A], z: Jist[A])(f: (A, A) => A): Jist[A] =
      (x, y) match {
        case (EmptyJist, EmptyJist)  => z
        case (Cons(_, _), EmptyJist) => concat(z, x)
        case (EmptyJist, Cons(_, _)) => concat(z, y)
        case (Cons(ah, at), Cons(bh, bt)) =>
          loop(at, bt, append(f(ah, bh), z))(f)
      }

    loop(a, b, EmptyJist)(f)
  }

  // def hasSubsequence[A](sup: Jist[A], sub: Jist[A]): Boolean =
	// 	sup match {
	// 		case EmptyJist => false
	// 		case Cons(h, t) =>
	// 			val supLen = Jist.length(sup)
	// 			val subLen = Jist.length(sub)
	// 			val lenDif = supLen - subLen

	// 			val first = Jist.drop(sup, lenDif)
	// 			val second = Jist.dropLastN(sup, lenDif)

	// 			(sub == first) || (sub == second)
	// 	}
}

sealed trait Jist[+A]
case object EmptyJist extends Jist[Nothing]
case class Cons[+A](head: A, tail: Jist[A]) extends Jist[A]
