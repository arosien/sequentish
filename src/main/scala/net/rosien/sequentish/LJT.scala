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
    def unapply(terms: List[Term]): Option[(Term.And, List[Term])] =
      terms.collectFirst { case t: Term.And => t -> terms.diff(List(t)) }
  }
  object or {
    def unapply(terms: List[Term]): Option[(Term.Or, List[Term])] =
      terms.collectFirst { case t: Term.Or => t -> terms.diff(List(t)) }
  }

  implicit val prover: Prover[LJT] = new Prover[LJT] {
    def rules =
      NonEmptySet.of(
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
    def prove(
        rule: LJT,
        sequent: Sequent
    ): Prover.Step[LJT] =
      rule match {
        case Id =>
          Prover.Step(rule, sequent) {
            case Sequent(ps, c) if ps contains c =>
              Prover.Step.Discharged(rule)
          }
        case `L⊥` =>
          Prover.Step(rule, sequent) {
            case Sequent(ps, _) if ps contains Term.False =>
              Prover.Step.Discharged(rule)
          }
        case `L∧` =>
          Prover.Step(rule, sequent) {
            case Sequent(and(Term.And(a, b), g), h) =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(a :: b :: g, h)))
          }
        case `L∨` =>
          Prover.Step(rule, sequent) {
            case Sequent(or(Term.Or(a, b), g), h) =>
              Prover.Step.And(
                rule,
                NonEmptyList.of(Sequent(a :: g, h), Sequent(b :: g, h))
              )
          }
        case `L⇒1` =>
          Prover.Step(rule, sequent) {
            case Sequent(
                (a1: Term.Atomic) :: Term.Implies(a2: Term.Atomic, b) :: g,
                c
                ) if a1 == a2 =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(a1 :: b :: g, c)))
          }
        case `L⇒2` =>
          Prover.Step(rule, sequent) {
            case Sequent(Term.Implies(Term.And(a, b), c) :: g, h) =>
              Prover.Step.And(
                rule,
                NonEmptyList
                  .of(Sequent(Term.Implies(a, Term.Implies(b, c)) :: g, h))
              )
          }
        case `L⇒3` =>
          Prover.Step(rule, sequent) {
            case Sequent(Term.Implies(Term.Or(a, b), c) :: g, h) =>
              Prover.Step.And(
                rule,
                NonEmptyList
                  .of(Sequent(Term.Implies(a, c) :: Term.Implies(b, c) :: g, h))
              )
          }
        case `L⇒4` =>
          Prover.Step(rule, sequent) {
            case Sequent(Term.Implies(Term.Implies(a, b), c) :: g, d) =>
              Prover.Step.and(
                rule,
                Sequent(Term.Implies(b, c) :: g, Term.Implies(a, b)),
                Sequent(List(c), d)
              )
          }
        case `R⇒` =>
          Prover.Step(rule, sequent) {
            case Sequent(g, Term.Implies(a, b)) =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(a :: g, b)))
          }
        case `R∧` =>
          Prover.Step(rule, sequent) {
            case Sequent(g, Term.And(a, b)) =>
              Prover.Step
                .And(rule, NonEmptyList.of(Sequent(g, a), Sequent(g, b)))
          }
        case `R∨1` =>
          Prover.Step(rule, sequent) {
            case Sequent(g, Term.Or(a, _)) =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(g, a)))
          }
        case `R∨2` =>
          Prover.Step(rule, sequent) {
            case Sequent(g, Term.Or(_, b)) =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(g, b)))
          }
      }
  }
}
