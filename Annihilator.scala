package underscore.annihilator

trait Annihilator[A, R] {
  def zero: A
  def product(a1: A, a2: A): A

  def refine[B](a: A)(then: R => B)(otherwise: => B): B
  def unrefine(in: R): A

  def and[B](a1: A, a2: A)(then: (R, R) => B)(otherwise: => B): B =
    refine(a1){ r1 =>
      refine(a2){ r2 =>
        then(r1, r2)
      }{
        otherwise
      }
    }{
     otherwise
    }

  def toOption(a: A): Option[R] =
    refine(a)(r => Option(r))(None)
  def fromOption(o: Option[R]): A =
    o.fold(zero)(unrefine _) 
}

trait AnnihilatorSyntax {
  implicit class AnnihilatorOps[A, R](a: A)(implicit annihilator: Annihilator[A, R]) {
    def refine[B](then: R => B)(otherwise: => B): B =
      annihilator.refine(a)(then)(otherwise)

    def and[B](that: A)(then: (R, R) => B)(otherwise: => B): B =
      annihilator.and(a, that)(then)(otherwise)

    def toOption: Option[R] =
      annihilator.toOption(a)
  }

  implicit class UnrefineOps[A, R](r: R)(implicit annihilator: Annihilator[A, R]) {
    def unrefine: A =
      annihilator.unrefine(r)
  }

  implicit class FromOptionOps[A, R](optR: Option[R])(implicit annihilator: Annihilator[A, R]) {
    def fromOption: A =
      annihilator.fromOption(optR)
  }
}
