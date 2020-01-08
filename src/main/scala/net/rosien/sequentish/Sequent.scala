package net.rosien.sequentish

import cats._
import cats.implicits._

case class Sequent(premises: List[Term], conclusion: Term)

object Sequent {
  def apply(term: Term): Sequent = Sequent(Nil, term)
  def apply(premise: Term, conclusion: Term): Sequent =
    Sequent(List(premise), conclusion)
  def apply[A: ToTerm]: Sequent = Sequent(ToTerm[A].toTerm)
  def apply[A: ToTerm, B: ToTerm]: Sequent =
    Sequent(ToTerm[A].toTerm, ToTerm[B].toTerm)

  implicit val show: Show[Sequent] = {
    case Sequent(ps, c) => show"${ps.mkString_(", ")} ⊢ $c"
  }
  
  case object Discharged {
    implicit val show: Show[Discharged.type] = Show.fromToString
  }
}
