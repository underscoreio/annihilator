package underscore.annihilator

import cats.Order

sealed trait Interval[A] extends AnnihilatorSyntax {
  import Interval._

  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    (this and that){ (i1, i2) =>
      val start = order.max(i1.start, i2.start)
      val end = order.min(i1.end, i2.end)
      nonempty[A](start, end)
    }{
      empty
    }
}
final case class Nonempty[A](start: A, end: A) extends Interval[A]
final case class Empty[A]() extends Interval[A]

object Interval {
  // "Smart" constructors

  def nonempty[A](start: A, end: A): Interval[A] =
    Nonempty(start, end)

  def empty[A]: Interval[A] =
    Empty()

  // Type class instances

  implicit def intervalAnnihilator[A : Order]: Annihilator[Interval[A], Nonempty[A]] = {
    object annihilator extends Annihilator[Interval[A], Nonempty[A]] {
      val zero = empty[A]
      def product(a1: Interval[A], a2: Interval[A]): Interval[A] =
        a1 intersect a2

      def refine[B](a: Interval[A])(then: Nonempty[A] => B)(otherwise: => B): B =
        a match {
          case Empty() => otherwise
          case refined @ Nonempty(_, _) => then(refined)
        }
      def unrefine(in: Nonempty[A]): Interval[A] = in
    }

    annihilator
  }
}
