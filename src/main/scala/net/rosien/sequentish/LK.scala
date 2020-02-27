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

  implicit val system: System[LK] =
    System(NonEmptySet.of[LK](Id, `L⇒`, `R⇒`))

  implicit val deducer: Deducer[LK] =
    new Deducer[LK] {
      def deduce(rule: LK): PartialFunction[Sequent, Deduction[LK]] =
        rule match {
          case LK.Id => {
            case Sequent(ps, c) if ps contains c =>
              Deduction.Discharged(rule)
          }
          case LK.`L⇒` => {
            case Sequent(Term.Implies(a, b) :: g, c) =>
              Deduction.Success(
                rule,
                NonEmptyList
                  .of(
                    Sequent(Term.Implies(a, b) :: g, a),
                    Sequent(List(b), c)
                  )
              )
          }
          case LK.`R⇒` => {
            case Sequent(g, Term.Implies(a, b)) =>
              Deduction.Success(rule, NonEmptyList.of(Sequent(a :: g, b)))
          }
        }
    }

  implicit val prover: Prover[LK] = Prover(system)
}
