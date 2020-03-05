package net.rosien.sequentish

import cats._
import cats.data._
import cats.implicits._

/**
  * https://nms.kcl.ac.uk/christian.urban/Prover/G4ip.html
  * https://pdfs.semanticscholar.org/4028/08025049cc2bae569f53f8e197ff4793a7fc.pdf
  */
sealed trait LJT

// TODO: Use enumeratum
object LJT {
  case object Id extends LJT
  case object `L⊥` extends LJT
  case object `L∧` extends LJT
  case object `L∨` extends LJT
  case object `L⇒1` extends LJT
  case object `L⇒2` extends LJT
  case object `L⇒3` extends LJT
  case object `L⇒4` extends LJT
  case object `R⇒` extends LJT
  case object `R∧` extends LJT
  case object `R∨1` extends LJT
  case object `R∨2` extends LJT

  implicit val show: Show[LJT] = Show.fromToString
  implicit val order: Order[LJT] = Order.by(_.getClass().getSimpleName())

  // TODO: more generic extractors?
  // TODO: perhaps produce disjunctions of conjunctions for multiple matches?
  object and {
    def unapply(terms: List[Formula]): Option[(Formula.And, List[Formula])] =
      terms.collectFirst { case t: Formula.And => t -> terms.diff(List(t)) }
  }
  object or {
    def unapply(terms: List[Formula]): Option[(Formula.Or, List[Formula])] =
      terms.collectFirst { case t: Formula.Or => t -> terms.diff(List(t)) }
  }

  implicit val system = System(
    NonEmptySet.of[LJT](
      Id,
      `L⊥`,
      `L∧`,
      `L∨`,
      `L⇒1`,
      `L⇒2`,
      `L⇒3`,
      `L⇒4`,
      `R⇒`,
      `R∧`,
      `R∨1`,
      `R∨2`
    )
  )

  implicit val deducer: Deducer[LJT] =
    new Deducer[LJT] {
      def deduce(rule: LJT): PartialFunction[Sequent, Deduction[LJT]] =
        rule match {
          case Id => {
            case Sequent(ps, c) if ps contains c =>
              Deduction.Discharged(rule)
          }
          case `L⊥` => {
            case Sequent(ps, _) if ps contains Formula.False =>
              Deduction.Discharged(rule)
          }
          case `L∧` => {
            case Sequent(and(Formula.And(a, b), g), h) =>
              Deduction.Success(rule, NonEmptyList.of(Sequent(a :: b :: g, h)))
          }
          case `L∨` => {
            case Sequent(or(Formula.Or(a, b), g), h) =>
              Deduction.Success(
                rule,
                NonEmptyList.of(Sequent(a :: g, h), Sequent(b :: g, h))
              )
          }
          case `L⇒1` => {
            case Sequent(
                (a1: Formula.Atomic) :: Formula.Implies(a2: Formula.Atomic, b) :: g,
                c
                ) if a1 == a2 =>
              Deduction.Success(rule, NonEmptyList.of(Sequent(a1 :: b :: g, c)))
          }
          case `L⇒2` => {
            case Sequent(Formula.Implies(Formula.And(a, b), c) :: g, h) =>
              Deduction.Success(
                rule,
                NonEmptyList
                  .of(Sequent(Formula.Implies(a, Formula.Implies(b, c)) :: g, h))
              )
          }
          case `L⇒3` => {
            case Sequent(Formula.Implies(Formula.Or(a, b), c) :: g, h) =>
              Deduction.Success(
                rule,
                NonEmptyList
                  .of(
                    Sequent(Formula.Implies(a, c) :: Formula.Implies(b, c) :: g, h)
                  )
              )
          }
          case `L⇒4` => {
            case Sequent(Formula.Implies(Formula.Implies(a, b), c) :: g, d) =>
              Deduction.Success(
                rule,
                NonEmptyList.of(
                  Sequent(Formula.Implies(b, c) :: g, Formula.Implies(a, b)),
                  Sequent(List(c), d)
                )
              )
          }
          case `R⇒` => // tag::Rimp[] */
            {
              case Sequent(g, Formula.Implies(a, b)) =>
                Deduction.Success(rule, NonEmptyList.of(Sequent(a :: g, b)))
            }
          /* end::Rimp[] */
          case `R∧` => {
            case Sequent(g, Formula.And(a, b)) =>
              Deduction
                .Success(rule, NonEmptyList.of(Sequent(g, a), Sequent(g, b)))
          }
          case `R∨1` => {
            case Sequent(g, Formula.Or(a, _)) =>
              Deduction.Success(rule, NonEmptyList.of(Sequent(g, a)))
          }
          case `R∨2` => {
            case Sequent(g, Formula.Or(_, b)) =>
              Deduction.Success(rule, NonEmptyList.of(Sequent(g, b)))
          }
        }
    }

  implicit val prover: Prover[LJT] = Prover(system)
}
