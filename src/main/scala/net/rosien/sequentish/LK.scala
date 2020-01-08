package net.rosien.sequentish

import cats._
import cats.data._
import cats.implicits._

/** https://en.wikipedia.org/wiki/Sequent_calculus#The_system_LK */
sealed trait LK

// TODO: Use enumeratum
object LK {

  /**
    * -------- Id
    * Γ, A ⊢ A
    */
  case object Id extends LK

  /**
    * Γ, A ⇒ B ⊢ A  Γ, B ⊢ C
    * ----------------------  L⇒
    *     Γ, A ⇒ B ⊢ C
    */
  case object `L⇒` extends LK

  /**
    * Γ, A ⊢ B
    * --------- R⇒
    * Γ ⊢ A ⇒ B
    */
  case object `R⇒` extends LK

  // TODO: L+, L*, R+, R*

  implicit val show: Show[LK] = Show.fromToString
  implicit val order: Order[LK] = Order.by(_.getClass().getSimpleName())

  implicit val prover: Prover[LK] = new Prover[LK] {
    def rules = NonEmptySet.of(Id, `L⇒`, `R⇒`)
    def prove(
        rule: LK,
        sequent: Sequent
    ): Prover.Step[LK] =
      rule match {
        case LK.Id =>
          Prover.Step(rule, sequent) {
            case Sequent(ps, c) if ps contains c =>
              Prover.Step.Discharged(rule)
          }
        case LK.`L⇒` =>
          Prover.Step(rule, sequent) {
            case Sequent(Term.Implies(a, b) :: g, c) =>
              Prover.Step.And(
                rule,
                NonEmptyList
                  .of(Sequent(Term.Implies(a, b) :: g, a), Sequent(List(b), c))
              )
          }
        case LK.`R⇒` =>
          Prover.Step(rule, sequent) {
            case Sequent(g, Term.Implies(a, b)) =>
              Prover.Step.And(rule, NonEmptyList.of(Sequent(a :: g, b)))
          }
      }
  }
}
