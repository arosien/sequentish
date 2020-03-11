package net.rosien.sequentish

import cats._
import cats.data._
import cats.implicits._

// TODO: Use enumeratum

/** https://en.wikipedia.org/wiki/Sequent_calculus#The_system_LK */
// tag::lk[]
sealed trait LK

object LK {

  /**
    * -------- Id
    * Γ, A ⊢ A
    */
  case object Id extends LK
  // end::lk[]

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

  case object `L∧` extends LK
  // case object `L∧1` extends LK
  // case object `L∧2` extends LK
  case object `R∧` extends LK

  case object `L∨` extends LK
  case object `R∨1` extends LK
  case object `R∨2` extends LK

  implicit val show: Show[LK] = Show.fromToString
  implicit val order: Order[LK] = Order.by(_.getClass().getSimpleName())

  implicit val system: System[LK] =
    System(NonEmptySet.of[LK](Id, `L∧`, `R∨1`, `R∨2`, `L⇒`, `R⇒`))

  implicit val deducer: Deducer[LK] =
    Deducer.fromPFs { rule =>
      rule match {
        // tag::id[]
        case Id => {
          case Sequent(ps, c) if ps contains c =>
            Deduction.Discharged(rule)
        }
        // end::id[]
        case `L∧` => {
          case Sequent(Formula.firstAnd(Formula.And(a, b), g), h) =>
            Deduction.Success(
              rule,
              NonEmptyList.of(Sequent(a :: b :: g, h))
            )
        }
        case `R∧` => ???
        case `L∨` => ???
        case `R∨1` => {
          case Sequent(g, Formula.Or(a, _)) =>
            Deduction.Success(rule, NonEmptyList.of(Sequent(g, a)))
        }
        case `R∨2` => {
          case Sequent(g, Formula.Or(_, b)) =>
            Deduction.Success(rule, NonEmptyList.of(Sequent(g, b)))
        }
        case `L⇒` => {
          case Sequent(Formula.Implies(a, b) :: g, c) =>
            Deduction.Success(
              rule,
              NonEmptyList
                .of(
                  Sequent(Formula.Implies(a, b) :: g, a),
                  Sequent(List(b), c)
                )
            )
        }
        case `R⇒` => {
          case Sequent(g, Formula.Implies(a, b)) =>
            Deduction.Success(rule, NonEmptyList.of(Sequent(a :: g, b)))
        }
      }
    }

  implicit val prover: Prover[LK] = Prover(system)
}
