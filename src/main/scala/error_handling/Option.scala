package error_handling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
			case None => None
			case Some(a) => Some(f(a))
	}

  def flatMap[B](f: A => Option[B]): Option[B] =
		if(this.equals(None)) None
		else f(this.asInstanceOf[Some[A]].get)

  def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(a) => a
	}

  def orElse[B >: A](ob: => Option[B]): Option[B] =
		if(this == None) ob
		else this

  def filter(f: A => Boolean): Option[A] =
		if(this.equals(None)) None
		else if(f(this.asInstanceOf[Some[A]].get)) this
		else None
}

final case object None extends Option[Nothing]
final case class Some[+A](get: A) extends Option[A]
