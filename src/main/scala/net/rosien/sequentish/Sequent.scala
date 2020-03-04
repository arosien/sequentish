package net.rosien.sequentish

import cats._
import cats.implicits._

// tag::sequent[]
case class Sequent(premises: List[Formula], conclusion: Formula)
// end::sequent[]

object Sequent {
  def apply(formula: Formula): Sequent = Sequent(Nil, formula)
  def apply(premise: Formula, conclusion: Formula): Sequent =
    Sequent(List(premise), conclusion)
  def apply[A: ToFormula]: Sequent = Sequent(ToFormula[A].toFormula)
  def apply[A: ToFormula, B: ToFormula]: Sequent =
    Sequent(ToFormula[A].toFormula, ToFormula[B].toFormula)

  implicit val show: Show[Sequent] = {
    case Sequent(ps, c) => show"${ps.mkString_(", ")} ⊢ $c"
  }
}
