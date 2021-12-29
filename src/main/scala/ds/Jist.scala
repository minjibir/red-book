package ds

object Jist {

  def apply[A](a: A*): Jist[A] =
    if (a.isEmpty) EmptyJist
    else Cons(a.head, apply(a.tail: _*))

  def sum(list: Jist[Int]): Int = {

    @annotation.tailrec
    def loop(xs: Jist[Int], acc: Int): Int = xs match {
      case EmptyJist  => acc
      case Cons(h, t) => loop(t, acc + h)
    }

    loop(list, 0)

  }

  def product(list: Jist[Int]): Int = {

    @annotation.tailrec
    def loop(lst: Jist[Int], acc: Int): Int = lst match {
      case EmptyJist  => acc
      case Cons(h, t) => loop(t, acc * h)
    }

    loop(list, 1)

  }

  def tail[A](list: Jist[A]): Jist[A] = list match {
    case EmptyJist     => EmptyJist
    case Cons(_, tail) => tail
  }

  def setHead[A](list: Jist[A], head: A): Jist[A] = list match {
    case EmptyJist   => Cons(head, EmptyJist)
    case Cons(_, xs) => Cons(head, xs)
  }

  def append[A](a: A, list: Jist[A]): Jist[A] = list match {
    case EmptyJist   => Cons(a, EmptyJist)
    case Cons(x, xs) => Cons(x, append(a, xs))
  }

  def concat[A](a: Jist[A], b: Jist[A]): Jist[A] = (a, b) match {
    case (EmptyJist, EmptyJist)     => EmptyJist
    case (Cons(_, _), EmptyJist)    => a
    case (EmptyJist, Cons(_, _))    => b
    case (Cons(x, xs), Cons(_, _)) => Cons(x, concat(xs, b))
  }

  @annotation.tailrec
  def drop[A](l: Jist[A], n: Int): Jist[A] = {
    if (l == EmptyJist || n <= 0) l
    else drop(tail(l), n - 1)
  }

//		@annotation.tailrec
  def dropWhile[A](l: Jist[A])(f: A => Boolean): Jist[A] = l match {
    case Cons(h, t) if f(h)  =>dropWhile(t)(f)
    case _ => l
    // def loop(jist: Jist[A], f: A => Boolean, state: Jist[A] = EmptyJist): Jist[A] =
    //   jist match {
    //     case EmptyJist => state
    //     case Cons(h, t) if (!f(h)) => state
    //     case Cons(h, t) => loop(t, f, t)
    //   }

    // loop(l, f, l)
  }

  /*
		Not everything works out so nicely. Implement a function, init, that returns a Jist
		consisting of all but the last element of a Jist. So, given Jist(1,2,3,4), init will
		return Jist(1,2,3). Why can’t this function be implemented in constant time like
		tail?
   */
  def init[Int](list: Jist[Int]): Jist[Int] = list match {
    case EmptyJist                   => EmptyJist
    case Cons(_, EmptyJist)          => EmptyJist
    case Cons(x, Cons(_, EmptyJist)) => Cons(x, EmptyJist)
    case Cons(x, Cons(y, ys))        => Cons(x, Cons(y, init(ys)))
  }
}

sealed trait Jist[+A]
case object EmptyJist extends Jist[Nothing]
case class Cons[+A](head: A, tail: Jist[A]) extends Jist[A]
