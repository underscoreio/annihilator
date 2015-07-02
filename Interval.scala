package underscore.annihilator

import cats.Order

sealed trait Interval[A] extends AnnihilatorSyntax {
  import cats.syntax.order._
  import Interval._

  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    (this and that){ (i1, i2) =>
      if((i2.end compare i1.start) < 0 || (i1.end compare i2.start) < 0) {
        empty[A]
      } else {
        val start = i1.start max i2.start
        val end = i1.end max i2.end
        nonempty(start, end)
      }
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
