package error_handling

sealed trait Option[A]
case object None extends Option[Nothing]
case class Some[A]() extends Option[A]
